const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate initial slice
    var slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;

    // Realloc to larger size
    slice = allocator.realloc(slice, 20) catch return 2;
    slice[0] = 43;

    // Free the reallocated slice
    allocator.free(slice);

    return 0;
}
