const std = @import("std");

pub fn main() !void {
    const fd = try std.posix.epoll_create1(0);
    _ = fd;
    // ERROR: epoll fd leaked
}
