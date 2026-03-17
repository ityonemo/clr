const std = @import("std");

const Container = struct {
    ptr: *u8,
};

fn freeContainer(container: *Container) void {
    // Use page_allocator directly to maintain allocator type tracking
    std.heap.page_allocator.destroy(container.ptr);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: Container = undefined;

    container.ptr = allocator.create(u8) catch return 1;
    container.ptr.* = 42;
    const val = container.ptr.*;

    freeContainer(&container);

    return val - 42;
}
