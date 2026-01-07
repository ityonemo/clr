// Test that storing heap pointer in global is fine
const std = @import("std");

var global_ptr: ?*u8 = null;

fn store_heap_ptr() void {
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
    store_heap_ptr();
    const result = use_global();
    free_global();
    return result;
}
