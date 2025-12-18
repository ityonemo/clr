const std = @import("std");

fn consumer(allocator: std.mem.Allocator, ptr: *u8) void {
    // Callee receives pointer and frees it - no leak
    ptr.* = 42;
    allocator.destroy(ptr);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    consumer(allocator, ptr);
    // Callee freed it - no leak
    return 0;
}
