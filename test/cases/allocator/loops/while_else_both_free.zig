// Test: correct cleanup pattern - free in both break and else paths
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        if (ptr.* == 42) {
            allocator.destroy(ptr);
            break;
        }
    } else {
        allocator.destroy(ptr);
    }
    return 0;
}
