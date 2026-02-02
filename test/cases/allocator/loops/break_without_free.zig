// Allocate in loop then break without freeing - memory leak
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;
        ptr.* = i;

        if (i == 2) {
            break; // Leak! ptr not freed before break
        }

        allocator.destroy(ptr);
    }

    return 0;
}
