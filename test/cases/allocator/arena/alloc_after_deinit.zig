const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

    const ptr1 = arena.allocator().create(u8) catch return 1;
    ptr1.* = 42;

    arena.deinit();

    // ERROR: allocating from deinited arena
    const ptr2 = arena.allocator().create(u8) catch return 1;
    ptr2.* = 99;

    return 0;
}
