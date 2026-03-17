const std = @import("std");

pub fn main() !void {
    var fd: std.posix.fd_t = undefined;
    _ = &fd; // prevent "never mutated" warning
    var buf: [10]u8 = undefined;
    _ = std.posix.read(fd, &buf) catch 0; // ERROR: reading from undefined fd
}
