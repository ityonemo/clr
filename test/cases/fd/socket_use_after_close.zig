const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
    std.posix.close(fd);
    var buf: [1]u8 = undefined;
    _ = std.posix.read(fd, &buf) catch 0; // ERROR: use-after-close
}
