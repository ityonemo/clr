const std = @import("std");

fn useContainer(container: *[1]*u8) void {
    // Use but don't free - causes leak in caller
    container[0].* = 99;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: [1]*u8 = undefined;
    _ = &container;

    container[0] = allocator.create(u8) catch return 1;
    container[0].* = 42;

    useContainer(&container);

    // Memory leak - callee didn't free, caller doesn't either
    return 0;
}
