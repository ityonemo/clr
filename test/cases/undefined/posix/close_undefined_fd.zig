const std = @import("std");

pub fn main() !void {
    var fd: std.posix.fd_t = undefined;
    _ = &fd; // prevent "never mutated" warning
    std.posix.close(fd); // ERROR: closing undefined fd
}
