const std = @import("std");

pub fn main() !void {
    const fd1 = try std.posix.open("/tmp/clr_test_dup2_1", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    const fd2 = try std.posix.open("/tmp/clr_test_dup2_2", .{ .ACCMODE = .RDWR, .CREAT = true }, 0o644);
    // dup2 replaces fd2 with a copy of fd1 (fd2 now points to same file as fd1)
    try std.posix.dup2(fd1, fd2);
    std.posix.close(fd1);
    std.posix.close(fd2);
    std.posix.close(fd2); // ERROR: double-close on fd2
}
