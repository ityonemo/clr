const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // dupeZ creates a null-terminated copy
    const original: []const u8 = "hello";
    const duped = allocator.dupeZ(u8, original) catch return 1;

    // dupeZ returns [:0]u8 - sentinel-terminated slice
    // Modify the copy
    duped[0] = 'H';

    // Free the duplicated slice
    allocator.free(duped);

    return 0;
}
