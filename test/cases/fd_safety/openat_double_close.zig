const std = @import("std");

pub fn main() !void {
    const dir_fd = try std.posix.open("/tmp", .{ .ACCMODE = .RDONLY }, 0);
    const fd = try std.posix.openat(dir_fd, "clr_test_openat", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    std.posix.close(fd);
    std.posix.close(fd); // ERROR: double-close
    std.posix.close(dir_fd);
}
