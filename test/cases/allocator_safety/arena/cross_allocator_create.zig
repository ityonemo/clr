const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Allocate with page_allocator, not arena
    const ptr = std.heap.page_allocator.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: freeing page_allocator allocation with arena allocator
    arena.allocator().destroy(ptr);

    return 0;
}
