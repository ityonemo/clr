// Test std.mem.asBytes over a single scalar without treating the scalar itself
// as a region. The returned byte pointer should support byte indexing.
const std = @import("std");

pub fn main() u8 {
    var value: u32 = 0x01020304;
    const bytes = std.mem.asBytes(&value);

    if (bytes.len != @sizeOf(u32)) return 1;
    _ = bytes[1];

    return 0;
}
