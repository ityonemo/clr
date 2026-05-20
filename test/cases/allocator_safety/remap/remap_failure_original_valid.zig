const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var slice = try allocator.alloc(u8, 10);
    slice[0] = 42;

    // Remap fails - original is still valid
    if (allocator.remap(slice, 20)) |new_slice| {
        allocator.free(new_slice);
    } else {
        // Original is valid here - this is correct
        _ = slice[0];
        allocator.free(slice);
    }

    return 0;
}
