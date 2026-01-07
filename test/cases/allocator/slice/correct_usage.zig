const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const slice = allocator.alloc(u8, 3) catch return 1;
    slice[0] = 42;
    const value = slice[0];
    allocator.free(slice);
    // Correct usage: use before free, free exactly once
    return value - 42;
}
