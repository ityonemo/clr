// Allocate before loop, free after loop - should pass
// Memory lives across all iterations, freed at the end
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 0;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        ptr.* = i; // Use the allocation in loop
    }

    allocator.destroy(ptr); // Free after loop
    return 0; // Should pass
}
