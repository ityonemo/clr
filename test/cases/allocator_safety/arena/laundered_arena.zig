const std = @import("std");

fn createArena() std.heap.ArenaAllocator {
    return std.heap.ArenaAllocator.init(std.heap.page_allocator);
}

pub fn main() u8 {
    var arena = createArena();
    defer arena.deinit();

    const ptr = arena.allocator().create(u8) catch return 1;
    ptr.* = 42;
    return 0;
}
