const std = @import("std");

const VTable = struct {
    process: *const fn (*u8) void,
};

fn uses_ptr(ptr: *u8) void {
    _ = ptr.*;  // Use after free
}

fn ignores_ptr(ptr: *u8) void {
    _ = ptr;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42; // Initialize to avoid undefined value error
    allocator.destroy(ptr);

    const cond = true;
    const vtable: VTable = if (cond) .{ .process = &uses_ptr } else .{ .process = &ignores_ptr };
    vtable.process(ptr);  // Fnptr inside struct - one branch uses freed ptr

    return 0;
}
