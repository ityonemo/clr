const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.open("/tmp/clr_test_write_after_close", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    std.posix.close(fd);
    const buf = "hello";
    _ = std.posix.write(fd, buf) catch 0; // ERROR: use-after-close
}
