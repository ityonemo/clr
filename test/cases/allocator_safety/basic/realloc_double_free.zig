const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate initial slice
    const slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;

    // Realloc to larger size - this implicitly frees the old slice
    const new_slice = allocator.realloc(slice, 20) catch return 2;
    new_slice[0] = 43;

    // ERROR: Double free - trying to free the old slice that was already freed by realloc
    allocator.free(slice);

    allocator.free(new_slice);

    return 0;
}
