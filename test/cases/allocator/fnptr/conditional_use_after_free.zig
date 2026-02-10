// Test: Conditional fnptr - one callback uses freed ptr (fails), one ignores (succeeds)
const std = @import("std");

fn uses_ptr(ptr: *u8) u8 {
    return ptr.*; // Uses freed pointer - error expected
}

fn ignores_ptr(_: *u8) u8 {
    return 99; // Ignores pointer - no error
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);

    var condition: bool = true;
    _ = &condition;
    const fp: *const fn (*u8) u8 = if (condition) &uses_ptr else &ignores_ptr;
    return fp(ptr); // Only uses_ptr branch will fail
}
