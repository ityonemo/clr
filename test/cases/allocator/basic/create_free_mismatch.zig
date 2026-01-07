const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate with create (single item)
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    // Convert to a slice of one and try to free with free (slice) - MISMATCH!
    // free expects a slice from alloc, not a single item from create
    const slice: []u8 = @as([*]u8, @ptrCast(ptr))[0..1];
    allocator.free(slice);

    return 0;
}
