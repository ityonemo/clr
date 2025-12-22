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
    // Caller doesn't free - leak!
    return value - 42;
}
