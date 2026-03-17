const std = @import("std");

const Expr = union(enum) {
    number: i32,
    negate: *Expr,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const expr = allocator.create(Expr) catch return 1;
    expr.* = .{ .number = 42 };

    allocator.destroy(expr);
    allocator.destroy(expr); // Double free!

    return 0;
}
