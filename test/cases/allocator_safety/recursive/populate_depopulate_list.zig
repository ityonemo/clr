const std = @import("std");

const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Populate: create 2-element list
    const node1 = allocator.create(Node) catch return 1;
    node1.* = .{ .value = 1, .next = null };

    const node2 = allocator.create(Node) catch return 1;
    node2.* = .{ .value = 2, .next = node1 };

    // Depopulate: free in reverse order
    const temp = node2.next; // Save reference to node1
    allocator.destroy(node2);
    allocator.destroy(temp.?);

    return 0; // OK: both freed properly
}
