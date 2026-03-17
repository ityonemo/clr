const std = @import("std");

const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: Container = undefined;

    container = .{ .ptr = allocator.create(u8) catch return 1 };
    container.ptr.* = 42;

    allocator.destroy(container.ptr);

    // Use after free through union field
    return container.ptr.*;
}
