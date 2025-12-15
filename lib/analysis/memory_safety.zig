const std = @import("std");
const Slot = @import("../slots.zig").Slot;

pub const Meta = struct {
    function: []const u8,
    file: []const u8,
    line: u32,
    column: u32,
};

pub const MemorySafety = union(enum) {
    stack_ptr: Meta,

    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        tracked[index].memory_safety = .{ .stack_ptr = .{
            .function = func_name,
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
        } };
    }

    // Note: We don't track args as stack pointers because:
    // - If arg is a value type (e.g., u8), taking &arg requires a separate alloc
    // - If arg is a pointer type (e.g., *u8), returning it is fine (it's the caller's memory)
    // The &param case is handled by the alloc that Zig generates for parameter addresses.

    pub fn bitcast(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        // Bitcast just reinterprets the pointer type (e.g., *u8 -> *const u8)
        // Propagate stack_ptr metadata from source to result
        const src = payload.src orelse return;
        if (src >= tracked.len) return;
        if (tracked[src].memory_safety) |ms| {
            tracked[index].memory_safety = ms;
        }
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        const ptr = payload.ptr orelse return;
        // Propagate stack_ptr metadata from source to destination
        if (tracked[index].memory_safety) |ms| {
            tracked[ptr].memory_safety = ms;
        }
    }

    pub fn ret_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const src = payload.src orelse return;
        if (src >= tracked.len) return;

        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        if (tracked[src].memory_safety) |ms| {
            switch (ms) {
                .stack_ptr => |meta| {
                    if (std.mem.eql(u8, meta.function, func_name)) {
                        return ms.reportStackEscape(ctx);
                    }
                },
            }
        }
    }

    pub fn reportStackEscape(self: MemorySafety, ctx: anytype) error{StackPointerEscape} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("stack pointer escape in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        switch (self) {
            .stack_ptr => |meta| {
                ctx.print("pointer to stack memory created in {s} ({s}:{d}:{d})\n", .{
                    meta.function,
                    meta.file,
                    meta.line,
                    meta.column,
                });
            },
        }
        return error.StackPointerEscape;
    }
};
