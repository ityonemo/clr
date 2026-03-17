// Test recursive type memory tracking
const Node = struct {
    value: u32,
    next: ?*Node,
};

pub fn main() u8 {
    const allocator = @import("std").heap.page_allocator;

    const node = allocator.create(Node) catch return 1;
    node.* = .{ .value = 42, .next = null };

    allocator.destroy(node);
    return 0;
}
