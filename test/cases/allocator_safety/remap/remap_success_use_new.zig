const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const slice = try allocator.alloc(u8, 10);

    if (allocator.remap(slice, 20)) |new_slice| {
        // Correct: use new_slice, not old slice
        new_slice[0] = 42;
        allocator.free(new_slice);
    } else {
        allocator.free(slice);
    }

    return 0;
}
