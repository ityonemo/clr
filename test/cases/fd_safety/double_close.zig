const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_double_close", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    std.posix.close(fd);
    std.posix.close(fd); // ERROR: double-close
}
