const std = @import("std");

var global_fd: std.posix.fd_t = 0;

pub fn main() !void {
    global_fd = try std.posix.open("/tmp/clr_test_global_fd_store_leak", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
}
