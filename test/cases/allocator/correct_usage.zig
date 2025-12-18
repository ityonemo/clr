const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    const value = ptr.*;
    allocator.destroy(ptr);
    // Correct usage: use before free, free exactly once
    return value - 42;
}
