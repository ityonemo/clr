const std = @import("std");

pub fn main() u8 {
    var args = std.process.args();
    _ = args.skip();
    _ = args.next();
    return 0;
}
