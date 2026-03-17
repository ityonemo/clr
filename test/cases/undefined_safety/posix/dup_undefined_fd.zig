const std = @import("std");

pub fn main() !void {
    var fd: std.posix.fd_t = undefined;
    _ = &fd; // prevent "never mutated" warning
    _ = std.posix.dup(fd) catch {}; // ERROR: dup on undefined fd
}
