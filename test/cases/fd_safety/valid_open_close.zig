const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_valid", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    std.posix.close(fd);
    // OK: properly opened and closed
}
