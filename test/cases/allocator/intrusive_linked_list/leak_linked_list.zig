const std = @import("std");

// Intrusive linked list item - node embedded in data structure
const Item = struct {
    data: u8,
    next: ?*Item = null,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Head of the list
    var first: ?*Item = null;

    // Create and prepend item1
    const item1 = allocator.create(Item) catch return 1;
    item1.* = .{ .data = 1, .next = first };
    first = item1;

    // Create and prepend item2
    const item2 = allocator.create(Item) catch return 1;
    item2.* = .{ .data = 2, .next = first };
    first = item2;

    // ERROR: Neither item freed - memory leak
    return 0;
}
