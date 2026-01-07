const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // dupe creates a copy - must be freed
    const original: []const u8 = "hello";
    const duped = allocator.dupe(u8, original) catch return 1;
    duped[0] = 'H';

    // ERROR: Memory leak - not freeing duped slice
    return 0;
}
