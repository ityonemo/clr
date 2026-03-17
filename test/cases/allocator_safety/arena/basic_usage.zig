const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit(); // This frees all arena allocations

    const ptr = arena.allocator().create(u8) catch return 1;
    ptr.* = 42;
    // No need to destroy ptr - arena.deinit() handles it
    return 0;
}
