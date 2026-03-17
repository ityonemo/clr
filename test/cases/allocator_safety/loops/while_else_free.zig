// Test: memory freed only in else clause - leak if break taken
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        if (ptr.* == 42) break; // Break without freeing
    } else {
        allocator.destroy(ptr); // Only frees if no break
    }
    return 0; // Leak if break taken!
}
