const std = @import("std");

var global_slice: []const u8 = "hello";

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    // Attempting to free a global slice should be an error
    allocator.free(global_slice);
    return 0;
}
