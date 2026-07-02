const std = @import("std");

pub fn main() u8 {
    var args = std.process.args();
    _ = args.skip();
    const path = args.next() orelse return 1;

    const file = std.fs.cwd().openFile(path, .{}) catch return 1;
    const stat_result = file.stat();
    file.close();

    _ = stat_result catch return 1;
    return 0;
}
