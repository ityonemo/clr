const std = @import("std");

const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn freeContainer(container: *Container) void {
    std.heap.page_allocator.destroy(container.ptr);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: Container = undefined;

    container = .{ .ptr = allocator.create(u8) catch return 1 };
    container.ptr.* = 42;

    freeContainer(&container);
    allocator.destroy(container.ptr); // Double free - callee already freed

    return 0;
}
