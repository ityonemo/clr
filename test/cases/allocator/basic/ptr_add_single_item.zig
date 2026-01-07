const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    // ERROR: pointer arithmetic on single-item pointer
    const bad_ptr = @as([*]u8, @ptrCast(ptr)) + 1;
    _ = bad_ptr;
    allocator.destroy(ptr);
    return 0;
}
