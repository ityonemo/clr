const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);
    // Double free - should be detected
    allocator.destroy(ptr);
    return 0;
}
