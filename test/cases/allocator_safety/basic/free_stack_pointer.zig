const std = @import("std");

pub fn main() u8 {
    var x: u8 = 42;
    const ptr = &x; // Stack pointer
    const allocator = std.heap.page_allocator;
    // ERROR: trying to free stack memory with allocator
    allocator.destroy(ptr);
    return 0;
}
