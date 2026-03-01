const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    // Manual free is allowed - arena allocations can still be freed normally
    allocator.destroy(ptr);

    return 0;
}
