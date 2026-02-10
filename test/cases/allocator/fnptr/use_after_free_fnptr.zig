// Test: Function pointer callback receives pointer to freed memory
const std = @import("std");

fn use_ptr(ptr: *u8) u8 {
    return ptr.*; // Use after free should be detected
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);

    const fp: *const fn (*u8) u8 = &use_ptr;
    return fp(ptr); // Pass freed pointer through function pointer
}
