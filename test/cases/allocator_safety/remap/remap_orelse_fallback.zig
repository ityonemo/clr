const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const slice = try allocator.alloc(u8, 10);

    // Common pattern: remap or alloc new + copy + free old
    const new_slice = allocator.remap(slice, 20) orelse blk: {
        const new = try allocator.alloc(u8, 20);
        @memcpy(new[0..10], slice);
        allocator.free(slice); // Valid: remap failed, original valid
        break :blk new;
    };

    new_slice[0] = 42;
    allocator.free(new_slice);

    return 0;
}
