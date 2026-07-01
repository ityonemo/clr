const std = @import("std");

pub fn main() u8 {
    var args = std.process.args();
    _ = args.skip();
    const path = args.next() orelse return 1;

    var maybe_file: ?std.fs.File = null;
    if (args.next() != null) {
        maybe_file = std.fs.cwd().openFile(path, .{}) catch return 1;
    }

    if (maybe_file) |file| {
        file.close();
    }
    return 0;
}
