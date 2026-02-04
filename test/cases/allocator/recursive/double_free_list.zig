const std = @import("std");

const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const node = allocator.create(Node) catch return 1;
    node.* = .{ .value = 1, .next = null };
    allocator.destroy(node);
    allocator.destroy(node); // ERROR: double free
    return 0;
}
