const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate a pointer - the memory is undefined
    const ptr: *u8 = allocator.create(u8) catch return 1;

    // Read from the pointer before writing to it - undefined!
    const value = ptr.*;

    allocator.destroy(ptr);

    return value;
}
