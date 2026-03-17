// Use after free in loop - free then use on next iteration
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        if (i == 1) {
            allocator.destroy(ptr); // Free on iteration 1
        }
        if (i == 2) {
            _ = ptr.*; // Use-after-free on iteration 2
        }
    }

    return 0;
}
