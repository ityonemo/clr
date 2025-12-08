const std = @import("std");

fn hexChar(nibble: u4) u8 {
    const n: u8 = nibble;
    return if (n < 10) '0' + n else 'a' + n - 10;
}

fn writeHex(ptr: usize) void {
    var buf: [16]u8 = undefined;
    var i: usize = 16;
    var v = ptr;
    while (i > 0) {
        i -= 1;
        buf[i] = hexChar(@truncate(v & 0xF));
        v >>= 4;
    }
    _ = std.os.linux.write(2, &buf, 16);
}

/// Write a string directly to stderr using Linux syscall.
pub fn debug_print(s: []const u8) void {
    writeHex(@intFromPtr(s.ptr));
    var space: [1]u8 = .{' '};
    _ = std.os.linux.write(2, &space, 1);
    writeHex(s.len);
    var nl: [1]u8 = .{'\n'};
    _ = std.os.linux.write(2, &nl, 1);
    _ = std.os.linux.write(2, s.ptr, s.len);
}
