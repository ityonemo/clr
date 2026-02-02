// Allocate in outer loop, free in inner loop - double free on second outer iteration
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 2) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;
        ptr.* = i;

        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            allocator.destroy(ptr); // Double-free: freed multiple times per outer iteration
        }
    }

    return 0;
}
