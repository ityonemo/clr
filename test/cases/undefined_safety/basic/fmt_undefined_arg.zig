const std = @import("std");

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;
    _ = std.fmt.bufPrint(&.{}, "{}", .{x}) catch unreachable;
    return 0;
}
