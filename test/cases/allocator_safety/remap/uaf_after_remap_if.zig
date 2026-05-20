const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var slice = try allocator.alloc(u8, 10);

    // Remap - may move memory, invalidating original slice
    if (allocator.remap(slice, 20)) |new_slice| {
        // BUG: using old slice after remap succeeded
        slice[0] = 42; // UAF - old slice may be freed
        _ = new_slice;
    }

    allocator.free(slice);
    return 0;
}
