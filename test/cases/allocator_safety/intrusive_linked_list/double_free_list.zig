const std = @import("std");

// Intrusive linked list item - node embedded in data structure
const Item = struct {
    data: u8,
    next: ?*Item = null,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Create single item
    const item = allocator.create(Item) catch return 1;
    item.* = .{ .data = 1, .next = null };

    // Free once (correct)
    allocator.destroy(item);

    // Free again (ERROR: double free)
    allocator.destroy(item);

    return 0;
}
