const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.socket(std.posix.AF.INET, std.posix.SOCK.STREAM, 0);
    std.posix.close(fd);
    std.posix.close(fd); // ERROR: double-close
}
