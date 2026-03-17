const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    // Missing arena.deinit() - ERROR: arena leaked
    const ptr = arena.allocator().create(u8) catch return 1;
    _ = ptr;
    return 0;
}
