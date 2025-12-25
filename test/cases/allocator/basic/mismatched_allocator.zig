const std = @import("std");

pub fn main() u8 {
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);

    // Allocate with page_allocator (comptime constant)
    const ptr = std.heap.page_allocator.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator (runtime)
    fba.allocator().destroy(ptr);

    return 0;
}
