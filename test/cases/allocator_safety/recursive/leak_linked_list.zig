const std = @import("std");

const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const node1 = allocator.create(Node) catch return 1;
    node1.* = .{ .value = 1, .next = null };
    const node2 = allocator.create(Node) catch return 1;
    node2.* = .{ .value = 2, .next = node1 };
    // ERROR: Neither node freed - memory leak
    return 0;
}
