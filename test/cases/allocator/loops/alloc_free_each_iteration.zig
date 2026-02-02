// Allocate and free within same iteration - should pass
// Each iteration creates fresh memory and cleans it up
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;
        ptr.* = i;
        allocator.destroy(ptr); // Clean up each iteration
    }

    return 0; // Should pass - no leaks
}
