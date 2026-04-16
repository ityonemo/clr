// Test that std.mem.eql works on string literals without false positives.
// This exercises the sliceAsBytes path through std.mem.eql.

const std = @import("std");

pub fn main() u8 {
    const a = "hello";
    const b = "hello";

    // This should use eqlBytes internally when vectors are available
    if (std.mem.eql(u8, a, b)) {
        return 0;
    }
    return 1;
}
