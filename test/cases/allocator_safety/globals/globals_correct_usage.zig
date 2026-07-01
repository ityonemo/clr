// Test that correct global pointer usage doesn't false positive
const std = @import("std");

var global_ptr: ?*u8 = null;

fn allocate_global() void {
    const allocator = std.heap.page_allocator;
    global_ptr = allocator.create(u8) catch return;
    global_ptr.?.* = 42;
}

fn use_global() u8 {
    return global_ptr.?.*;
}

fn free_global() void {
    const allocator = std.heap.page_allocator;
    allocator.destroy(global_ptr.?);
}

pub fn main() u8 {
    allocate_global();
    const result = use_global();
    free_global();
    return result;
}
