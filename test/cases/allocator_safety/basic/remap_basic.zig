const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate initial slice
    const slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;

    // Remap to larger size - returns optional
    // If remap succeeds, old slice is freed and new_slice is valid
    // If remap fails, old slice is unchanged (but we'd need to free it)
    const new_slice = allocator.remap(slice, 20) orelse return 2;
    new_slice[0] = 43;
    allocator.free(new_slice);

    return 0;
}
