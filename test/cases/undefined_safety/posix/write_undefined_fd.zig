const std = @import("std");

pub fn main() !void {
    var fd: std.posix.fd_t = undefined;
    _ = &fd; // prevent "never mutated" warning
    const data = "hello";
    _ = std.posix.write(fd, data) catch 0; // ERROR: writing to undefined fd
}
