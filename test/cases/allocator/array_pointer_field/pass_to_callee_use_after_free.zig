const std = @import("std");

fn freeContainer(container: *[1]*u8) void {
    std.heap.page_allocator.destroy(container[0]);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: [1]*u8 = undefined;
    _ = &container;

    container[0] = allocator.create(u8) catch return 1;
    container[0].* = 42;

    freeContainer(&container);

    // Use after free - callee already freed
    return container[0].*;
}
