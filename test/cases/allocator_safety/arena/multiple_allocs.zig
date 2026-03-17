const std = @import("std");

pub fn main() u8 {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    // Multiple allocations - all freed by arena.deinit()
    const ptr1 = arena.allocator().create(u8) catch return 1;
    const ptr2 = arena.allocator().create(u32) catch return 1;
    const ptr3 = arena.allocator().create(u64) catch return 1;

    ptr1.* = 1;
    ptr2.* = 2;
    ptr3.* = 3;

    return 0;
}
