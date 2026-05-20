const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const slice = try allocator.alloc(u8, 10);

    if (allocator.remap(slice, 20)) |new_slice| {
        // BUG: neither old nor new freed
        _ = new_slice;
    }
    // Leak: slice is never freed in either branch

    return 0;
}
