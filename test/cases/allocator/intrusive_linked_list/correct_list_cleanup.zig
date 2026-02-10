const std = @import("std");

// Intrusive linked list item - node embedded in data structure
const Item = struct {
    data: u8,
    next: ?*Item = null,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Create item1
    const item1 = allocator.create(Item) catch return 1;
    item1.* = .{ .data = 1, .next = null };

    // Create item2, link to item1
    const item2 = allocator.create(Item) catch return 1;
    item2.* = .{ .data = 2, .next = item1 };

    // Destroy in reverse order (like the recursive test)
    allocator.destroy(item2);
    allocator.destroy(item1);

    return 0; // OK: both freed
}
