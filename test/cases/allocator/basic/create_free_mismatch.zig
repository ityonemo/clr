const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate with create (single item)
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    // Convert to a slice of one and try to free with free (slice) - MISMATCH!
    // free expects a slice from alloc, not a single item from create
    // Use *[1]u8 cast to avoid ptr_add (full array slice doesn't need arithmetic)
    const slice: []u8 = @as(*[1]u8, @ptrCast(ptr));
    allocator.free(slice);

    return 0;
}
