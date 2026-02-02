// Allocation in loop without free - memory leak
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;
        ptr.* = i;
        // Missing: allocator.destroy(ptr);
        // Each iteration leaks memory
    }

    return 0; // ERROR: memory leak
}
