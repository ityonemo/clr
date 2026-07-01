// Test that memory leak is detected for global pointer across function calls
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

pub fn main() u8 {
    allocate_global();
    // Leak: global pointer not freed before program ends
    return use_global();
}
