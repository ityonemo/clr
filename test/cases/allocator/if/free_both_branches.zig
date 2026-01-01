const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var condition: bool = true;
    _ = &condition;

    if (condition) {
        allocator.destroy(ptr);
    } else {
        allocator.destroy(ptr);
    }
    // Both branches free - no leak
    return 0;
}
