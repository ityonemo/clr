const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const ptr = arena.allocator().create(u8) catch return 1;
    ptr.* = 42;

    arena.deinit(); // Frees all arena allocations

    // ERROR: use after arena deinit
    return ptr.*;
}
