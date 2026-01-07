const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: [1]*u8 = undefined;
    _ = &container;

    container[0] = allocator.create(u8) catch return 1;
    container[0].* = 42;

    // Memory leak - not freeing container[0]
    return 0;
}
