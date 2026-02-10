const std = @import("std");

fn uses_ptr(ptr: *u8) void {
    _ = ptr.*;  // Use after free
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42; // Initialize to avoid undefined value error
    allocator.destroy(ptr);

    const callback: *const fn (*u8) void = &uses_ptr;
    callback(ptr);  // Passing freed pointer through fnptr

    return 0;
}
