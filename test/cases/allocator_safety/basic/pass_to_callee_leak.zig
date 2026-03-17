const std = @import("std");
const allocator = std.heap.page_allocator;

fn consumer(ptr: *u8) void {
    // Callee receives pointer but doesn't free it - should be a leak
    ptr.* = 42;
}

pub fn main() u8 {
    const ptr = allocator.create(u8) catch return 1;
    consumer(ptr);
    // No free - callee didn't free, caller didn't free = leak
    return 0;
}
