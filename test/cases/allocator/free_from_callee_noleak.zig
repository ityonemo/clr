const std = @import("std");
const allocator = std.heap.page_allocator;

fn producer() ?*u8 {
    const ptr = allocator.create(u8) catch return null;
    ptr.* = 42;
    return ptr;
}

pub fn main() u8 {
    const ptr = producer() orelse return 1;
    const value = ptr.*;
    // Caller frees what callee allocated - no leak
    allocator.destroy(ptr);
    return value - 42;
}
