const std = @import("std");

pub fn main() u8 {
    var args = std.process.args();
    _ = args.skip();
    const path = args.next() orelse return 1;

    var maybe_file: ?std.fs.File = std.fs.cwd().openFile(path, .{}) catch return 1;
    const file = maybe_file.?;
    file.close();
    maybe_file = null;
    return 0;
}
