const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const original: []const u8 = "hello";
    const duped = allocator.dupe(u8, original) catch return 1;
    duped[0] = 'H';

    allocator.free(duped);

    // ERROR: Use after free
    duped[0] = 'X';

    return 0;
}
