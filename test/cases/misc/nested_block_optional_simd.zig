// Test for nested block expansion in sub-functions.
// Uses std.mem.indexOfSentinel which triggers the pattern where a safety
// check block inside a cond_br branch has its body not expanded.

const std = @import("std");

pub fn main() u8 {
    var data: [10:0]u8 = .{ 'h', 'e', 'l', 'l', 'o', 0, 0, 0, 0, 0 };
    _ = &data;

    // indexOfSentinel uses SIMD internally which generates nested blocks
    const len = std.mem.indexOfSentinel(u8, 0, @as([*:0]const u8, &data));

    if (len == 5) {
        return 0;
    }
    return 1;
}
