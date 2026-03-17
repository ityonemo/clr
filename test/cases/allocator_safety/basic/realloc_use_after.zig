const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate initial slice
    const slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;

    // Realloc to larger size - this frees the old slice
    const new_slice = allocator.realloc(slice, 20) catch return 2;
    _ = new_slice;

    // ERROR: Use after realloc - slice points to freed memory
    slice[0] = 99;

    return 0;
}
