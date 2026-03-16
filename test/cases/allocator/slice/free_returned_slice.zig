// Test that freeing a slice returned from a function doesn't cause false positive.
// The callee allocates and returns, the caller frees - no leak should be reported.

const std = @import("std");
const allocator = std.heap.page_allocator;

fn getSlice() ![]u8 {
    return try allocator.alloc(u8, 10);
}

pub fn main() !u8 {
    const slice = try getSlice();
    allocator.free(slice);
    return 0;
}
