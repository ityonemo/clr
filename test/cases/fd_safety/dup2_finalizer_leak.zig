const std = @import("std");

fn duplicateToTarget(src: std.posix.fd_t, dst: std.posix.fd_t) !void {
    try std.posix.dup2(src, dst);
}

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_dup2_finalizer_leak", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    defer std.posix.close(fd);

    try duplicateToTarget(fd, 100);
}
