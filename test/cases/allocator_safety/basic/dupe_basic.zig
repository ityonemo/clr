const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // dupe creates a copy of the slice
    const original: []const u8 = "hello";
    const duped = allocator.dupe(u8, original) catch return 1;
    duped[0] = 'H'; // Can modify the copy

    // Free the duplicated slice
    allocator.free(duped);

    return 0;
}
