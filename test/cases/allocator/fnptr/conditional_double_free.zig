// Test: Conditional fnptr - one callback frees (double free), one doesn't
const std = @import("std");

fn does_free(allocator: std.mem.Allocator, ptr: *u8) void {
    allocator.destroy(ptr); // Double free - error expected
}

fn no_free(_: std.mem.Allocator, _: *u8) void {
    // Does nothing - no error
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr); // First free

    var condition: bool = true;
    _ = &condition;
    const fp: *const fn (std.mem.Allocator, *u8) void = if (condition) &does_free else &no_free;
    fp(allocator, ptr); // Only does_free branch will fail
    return 0;
}
