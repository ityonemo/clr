const std = @import("std");

const Container = struct {
    ptr: *u8,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var container: Container = undefined;

    container.ptr = allocator.create(u8) catch return 1;
    container.ptr.* = 42;
    const val = container.ptr.*;

    allocator.destroy(container.ptr);

    return val - 42;
}
