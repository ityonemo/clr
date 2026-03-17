// Test: early return on allocation error should not report leak
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // This pattern: allocation that might fail, early return on error
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);
    return 0;
}
