const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            allocator.destroy(ptr);
        },
        1 => {
            allocator.destroy(ptr);
        },
        else => {
            allocator.destroy(ptr);
        },
    }
    // After merge, ptr is freed in all cases
    // Freeing again should be double-free
    allocator.destroy(ptr);
    return 0;
}
