const std = @import("std");

pub fn main() u8 {
    // Allocate with page_allocator (comptime constant)
    const ptr = std.heap.page_allocator.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator (also comptime constant, but different type)
    std.heap.c_allocator.destroy(ptr);

    return 0;
}
