// Test: Function pointer callback performs double free
const std = @import("std");

fn do_free(allocator: std.mem.Allocator, ptr: *u8) void {
    allocator.destroy(ptr); // Double free should be detected
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr); // First free

    const fp: *const fn (std.mem.Allocator, *u8) void = &do_free;
    fp(allocator, ptr); // Double free through function pointer
    return 0;
}
