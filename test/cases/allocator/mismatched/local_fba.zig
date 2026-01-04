const std = @import("std");

pub fn main() u8 {
    // Local FixedBufferAllocator
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const alloc = fba.allocator();

    // Allocate with FBA
    const ptr = alloc.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator (page_allocator)
    std.heap.page_allocator.destroy(ptr);

    return 0;
}
