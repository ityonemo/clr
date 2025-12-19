const std = @import("std");

/// Generic metadata struct, to be used by various analytes.
function: []const u8,
file: []const u8,
line: u32,
column: ?u32 = null,

pub fn print(self: @This(), writer: anytype, comptime prefix: []const u8, prefix_args: anytype) !void {
    var buf: [1024]u8 = undefined;
    if (self.column) |column| {
        const fmt = comptime prefix ++ "{s} ({s}:{d}:{d})\n";
        const args = prefix_args ++ .{ self.function, self.file, self.line, column };
        const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
        try writer.writeAll(msg);
    } else {
        const fmt = comptime prefix ++ "{s} ({s}:{d})\n";
        const args = prefix_args ++ .{ self.function, self.file, self.line };
        const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
        try writer.writeAll(msg);
    }
}