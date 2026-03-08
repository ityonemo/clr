const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_dup_leak", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    const fd2 = try std.posix.dup(fd);
    std.posix.close(fd);
    _ = fd2;
    // ERROR: fd2 leaked (dup'd fd not closed)
}
