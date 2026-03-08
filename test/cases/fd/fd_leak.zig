const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_fd_leak", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    _ = fd;
    // ERROR: fd leaked (not closed)
}
