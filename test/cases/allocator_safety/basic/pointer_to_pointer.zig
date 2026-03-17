const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // 1. Allocate a pointer to a pointer
    const outer: **u8 = allocator.create(*u8) catch return 1;

    // 2. Allocate the inner pointer
    const inner: *u8 = allocator.create(u8) catch return 2;
    outer.* = inner;

    // 3. Set the value of the inner pointer
    inner.* = 42;

    const value = inner.*;

    // 4. Free the inner pointer
    allocator.destroy(inner);

    // 5. Free the outer pointer
    allocator.destroy(outer);

    return value - 42;
}
