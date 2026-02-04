const std = @import("std");

const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Create 2-element list
    const node1 = allocator.create(Node) catch return 1;
    node1.* = .{ .value = 1, .next = null };

    var root = allocator.create(Node) catch return 1;
    root.* = .{ .value = 2, .next = node1 };

    // Lose the root by reassigning - LEAK both nodes!
    root = allocator.create(Node) catch return 1;
    root.* = .{ .value = 3, .next = null };

    allocator.destroy(root); // Only frees the new node
    return 0; // ERROR: node1 and original root leaked
}
