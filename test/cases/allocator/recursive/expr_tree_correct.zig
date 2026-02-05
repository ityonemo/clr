const std = @import("std");

const Expr = union(enum) {
    number: i32,
    negate: *Expr,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Create: negate(42)
    const inner = allocator.create(Expr) catch return 1;
    inner.* = .{ .number = 42 };

    const outer = allocator.create(Expr) catch return 1;
    outer.* = .{ .negate = inner };

    // Clean up in reverse order
    allocator.destroy(outer);
    allocator.destroy(inner);

    return 0;
}
