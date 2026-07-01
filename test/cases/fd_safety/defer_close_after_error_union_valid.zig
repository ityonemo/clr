const std = @import("std");

pub fn main() u8 {
    var args = std.process.args();
    _ = args.skip();
    const path = args.next() orelse return 1;

    const file = std.fs.cwd().openFile(path, .{}) catch return 1;
    defer file.close();

    var buffer: [1]u8 = undefined;
    _ = file.read(&buffer) catch return 1;
    return 0;
}
