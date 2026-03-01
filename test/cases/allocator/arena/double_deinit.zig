const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const ptr = arena.allocator().create(u8) catch return 1;
    ptr.* = 42;

    arena.deinit(); // First deinit - ok
    // ERROR: second deinit via defer - double free

    return 0;
}
