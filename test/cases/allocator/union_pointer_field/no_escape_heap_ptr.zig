const std = @import("std");

const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn getContainer() !Container {
    const ptr = try std.heap.page_allocator.create(u8);
    ptr.* = 42;
    return .{ .ptr = ptr }; // OK: heap ptr in union is fine
}

pub fn main() !u8 {
    const container = try getContainer();
    const val = container.ptr.*;
    std.heap.page_allocator.destroy(container.ptr);
    return val - 42;
}
