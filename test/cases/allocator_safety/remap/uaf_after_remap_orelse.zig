const std = @import("std");

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var slice = try allocator.alloc(u8, 10);

    // Remap with orelse - success path invalidates original
    const new_slice = allocator.remap(slice, 20) orelse {
        // Failure path - original is still valid here
        allocator.free(slice);
        return 1;
    };

    // BUG: using old slice after remap succeeded
    slice[0] = 42; // UAF - old slice may be freed
    _ = new_slice;

    return 0;
}
