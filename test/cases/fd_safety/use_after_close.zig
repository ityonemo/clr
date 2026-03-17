const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_use_after_close", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    std.posix.close(fd);
    var buf: [1]u8 = undefined;
    _ = std.posix.read(fd, &buf) catch 0; // ERROR: use-after-close
}
