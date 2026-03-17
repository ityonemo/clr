const std = @import("std");

fn openFile() !std.posix.fd_t {
    return try std.posix.open("/tmp/clr_test_return", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    // OK: fd is returned, ownership transferred to caller
}

pub fn main() !void {
    const fd = try openFile();
    std.posix.close(fd);
    // OK: caller properly closes the returned fd
}
