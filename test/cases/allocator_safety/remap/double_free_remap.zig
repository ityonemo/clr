const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const slice = try allocator.alloc(u8, 10);

    if (allocator.remap(slice, 20)) |new_slice| {
        allocator.free(new_slice);
        // BUG: freeing old slice that was transferred to new_slice
        allocator.free(slice); // Double-free
    }

    return 0;
}
