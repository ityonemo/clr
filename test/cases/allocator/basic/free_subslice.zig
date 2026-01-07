const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;
    // Take a sub-slice
    const subslice = slice[2..5];
    // ERROR: trying to free sub-slice (not the original allocation)
    allocator.free(subslice);
    return 0;
}
