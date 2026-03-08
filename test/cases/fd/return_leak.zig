const std = @import("std");

fn openFile() !std.posix.fd_t {
    return try std.posix.open("/tmp/clr_test_return_leak", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
}

pub fn main() !void {
    const fd = try openFile();
    _ = fd;
    // ERROR: caller doesn't close the returned fd
}
