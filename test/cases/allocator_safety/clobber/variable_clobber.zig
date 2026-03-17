const std = @import("std");

pub fn main() !void {
    var ptr = try std.heap.page_allocator.create(u8);
    ptr = try std.heap.page_allocator.create(u8); // clobber! first allocation leaked
    std.heap.page_allocator.destroy(ptr);
}
