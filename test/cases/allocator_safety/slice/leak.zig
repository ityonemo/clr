const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const slice = allocator.alloc(u8, 3) catch return 1;
    slice[0] = 42;
    // Memory leak - not freeing slice
    return 0;
}
