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
    // After merge, ptr is freed in both branches
    // Freeing again should be double-free
    allocator.destroy(ptr);
    return 0;
}
