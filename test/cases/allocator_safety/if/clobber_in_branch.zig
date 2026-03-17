const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var condition: bool = true;
    _ = &condition;

    var ptr: *u8 = undefined;

    if (condition) {
        ptr = allocator.create(u8) catch return 1;
        ptr = allocator.create(u8) catch return 1; // clobber - first allocation leaked
    } else {
        ptr = allocator.create(u8) catch return 1;
    }

    allocator.destroy(ptr);
    return 0;
}
