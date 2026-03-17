const std = @import("std");

fn uses_ptr(ptr: *u8) void {
    _ = ptr.*;  // Use after free in this branch
}

fn ignores_ptr(ptr: *u8) void {
    _ = ptr;  // Does nothing with ptr
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42; // Initialize to avoid undefined value error
    allocator.destroy(ptr);

    var cond = true;
    _ = &cond;
    const callback: *const fn (*u8) void = if (cond) &uses_ptr else &ignores_ptr;
    callback(ptr);  // One possible branch uses freed ptr

    return 0;
}
