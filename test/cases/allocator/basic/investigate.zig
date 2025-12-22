const std = @import("std");

noinline fn useAllocator(allocator: std.mem.Allocator) u8 {
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;
    allocator.destroy(ptr);
    return 0;
}

pub fn main() u8 {
    return useAllocator(std.heap.page_allocator);
}
