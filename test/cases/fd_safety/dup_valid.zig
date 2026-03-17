const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_dup_valid", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    const fd2 = try std.posix.dup(fd);
    std.posix.close(fd);
    std.posix.close(fd2);
    // OK: both fds properly closed
}
