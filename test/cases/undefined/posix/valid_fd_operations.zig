const std = @import("std");

pub fn main() !void {
    // All operations use defined fds - no errors expected
    const fd = try std.posix.open("/tmp/clr_test_valid", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    const data = "hello";
    _ = std.posix.write(fd, data) catch 0;
    var buf: [10]u8 = undefined;
    _ = std.posix.read(fd, &buf) catch 0;
    std.posix.close(fd);
}
