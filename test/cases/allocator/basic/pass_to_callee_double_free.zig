const std = @import("std");
const allocator = std.heap.page_allocator;

fn consumer(ptr: *u8) void {
    // Callee frees the pointer
    ptr.* = 42;
    allocator.destroy(ptr);
}

pub fn main() u8 {
    const ptr = allocator.create(u8) catch return 1;
    consumer(ptr);
    // Caller also frees - double free!
    allocator.destroy(ptr);
    return 0;
}
