const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.epoll_create1(0);
    std.posix.close(fd);
    std.posix.close(fd); // ERROR: double-close
}
