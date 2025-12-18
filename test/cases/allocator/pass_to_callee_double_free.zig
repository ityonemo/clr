const std = @import("std");

fn consumer(allocator: std.mem.Allocator, ptr: *u8) void {
    // Callee frees the pointer
    ptr.* = 42;
    allocator.destroy(ptr);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    consumer(allocator, ptr);
    // Caller also frees - double free!
    allocator.destroy(ptr);
    return 0;
}
