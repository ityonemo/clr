const std = @import("std");
const compiler = @import("compiler");
const codegen = compiler.codegen;
const air = codegen.importBackend(.stage2_air);

pub const AllocatorVTable = air.AllocatorVTable;

// =============================================================================
// DLL Relocation Issue Background
// =============================================================================
//
// This module exists because standard library allocators don't work in
// dynamically loaded shared libraries (.so files). The root cause:
//
// 1. std.heap.page_allocator and std.heap.ArenaAllocator use compile-time
//    initialized vtables with function pointers.
//
// 2. When a .so is loaded via dlopen(), these compile-time pointers don't
//    get properly relocated - they point to invalid addresses (e.g., 0x8).
//
// 3. std.fmt.allocPrint also fails because it internally uses ArrayList
//    with a Writer vtable that has the same relocation problem.
//
// Solution: Accept an AllocatorVTable from the host process (which has
// correct addresses), and initialize all vtables at runtime in init().
// The AllocatorVTable uses C-ABI shims (callconv(.c)) to ensure stable
// function call conventions across the DLL boundary.
// =============================================================================

var stored_avt: AllocatorVTable = undefined;

// =============================================================================
// Arena Allocator Implementation
// =============================================================================
// Simple arena allocator that packs allocations into pages.
// Uses the host process's page_allocator via stored_avt.

const MAX_PAGES = 1024;
const PAGE_SIZE: usize = 4096;

// Shared vtable for all arenas - must be module-level var initialized at runtime
// to avoid DLL relocation issues with compile-time function pointers
var arena_vtable: std.mem.Allocator.VTable = undefined;

pub const Arena = struct {
    pages: [MAX_PAGES][*]u8 = undefined,
    page_count: usize = 0,
    current_page: ?[*]u8 = null,
    page_offset: usize = 0,

    pub fn init() Arena {
        return Arena{};
    }

    pub fn deinit(self: *Arena) void {
        for (self.pages[0..self.page_count]) |page| {
            stored_avt.free(null, page, PAGE_SIZE, 0, 0);
        }
        self.page_count = 0;
        self.current_page = null;
        self.page_offset = 0;
    }

    pub fn allocator(self: *Arena) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &arena_vtable,
        };
    }
};

fn arenaAlloc(ptr: *anyopaque, len: usize, ptr_align: std.mem.Alignment, _: usize) ?[*]u8 {
    const self: *Arena = @ptrCast(@alignCast(ptr));
    const alignment = @as(usize, 1) << @intFromEnum(ptr_align);

    // For large allocations, get dedicated pages
    if (len > PAGE_SIZE / 2) {
        const pages_needed = (len + PAGE_SIZE - 1) / PAGE_SIZE;
        const result = stored_avt.alloc(null, pages_needed * PAGE_SIZE, @intFromEnum(ptr_align), 0) orelse return null;
        if (self.page_count < MAX_PAGES) {
            self.pages[self.page_count] = result;
            self.page_count += 1;
        }
        return result;
    }

    // Align offset within current page
    const aligned_offset = (self.page_offset + alignment - 1) & ~(alignment - 1);

    // Get new page if needed
    if (self.current_page == null or aligned_offset + len > PAGE_SIZE) {
        const new_page = stored_avt.alloc(null, PAGE_SIZE, 0, 0) orelse return null;
        if (self.page_count < MAX_PAGES) {
            self.pages[self.page_count] = new_page;
            self.page_count += 1;
        }
        self.current_page = new_page;
        self.page_offset = 0;
        return arenaAlloc(ptr, len, ptr_align, 0);
    }

    const result = self.current_page.? + aligned_offset;
    self.page_offset = aligned_offset + len;
    return result;
}

fn arenaResize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
    return false;
}

fn arenaRemap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
    return null;
}

fn arenaFree(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {}

// Main arena instance
var main_arena: Arena = undefined;

/// Initialize the allocator with a vtable from the host process.
/// Must be called before using allocator().
pub fn init(avt: *const AllocatorVTable) void {
    stored_avt = avt.*;

    // Initialize shared vtable at runtime (compile-time vtables don't survive DLL relocation)
    arena_vtable = .{
        .alloc = arenaAlloc,
        .resize = arenaResize,
        .remap = arenaRemap,
        .free = arenaFree,
    };

    main_arena = Arena.init();
}

pub fn deinit() void {
    main_arena.deinit();
}

pub fn allocator() std.mem.Allocator {
    return main_arena.allocator();
}

/// Create a new arena for temporary allocations (e.g., per-function codegen)
pub fn newArena() Arena {
    return Arena.init();
}

/// Replacement for std.fmt.allocPrint that avoids DLL relocation issues.
/// std.fmt.allocPrint uses ArrayList with an internal Writer vtable, and
/// std.fmt.count uses Writer.Discarding - both have compile-time vtables
/// that fail in DLL context.
/// Instead: allocate a buffer, format with bufPrint, double on overflow.
pub fn allocPrint(alloc: std.mem.Allocator, comptime fmt: []const u8, args: anytype, size_hint: ?usize) ?[]u8 {
    var size: usize = size_hint orelse 4096;
    while (size <= 1024 * 1024) { // Max 1MB
        const buf = alloc.alloc(u8, size) catch return null;
        if (std.fmt.bufPrint(buf, fmt, args)) |result| {
            return result;
        } else |err| switch (err) {
            error.NoSpaceLeft => size *= 2,
        }
    }
    return null;
}
