const std = @import("std");

const Container = struct {
    ptr: *u8,
};

fn useContainer(container: *Container) u8 {
    // Uses the pointer but doesn't free it
    return container.ptr.*;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: Container = undefined;

    container.ptr = allocator.create(u8) catch return 1;
    container.ptr.* = 42;

    // Leak: function uses but doesn't free, and caller doesn't free either
    return useContainer(&container) - 42;
}
