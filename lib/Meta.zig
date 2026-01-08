const std = @import("std");

/// Generic metadata struct, to be used by various analytes.
function: []const u8,
file: []const u8,
line: u32,
column: ?u32 = null,

pub fn print(self: @This(), writer: anytype, comptime prefix: []const u8, prefix_args: anytype) !void {
    var buf: [1024]u8 = undefined;
    // Handle empty function names (e.g., for globals) - omit "in <function>" entirely
    const func_part = if (self.function.len > 0) self.function else "";
    if (self.column) |column| {
        if (func_part.len > 0) {
            const fmt = comptime prefix ++ "{s} ({s}:{d}:{d})\n";
            const args = prefix_args ++ .{ func_part, self.file, self.line, column };
            const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
            try writer.writeAll(msg);
        } else {
            const fmt = comptime prefix ++ "({s}:{d}:{d})\n";
            const args = prefix_args ++ .{ self.file, self.line, column };
            const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
            try writer.writeAll(msg);
        }
    } else {
        if (func_part.len > 0) {
            const fmt = comptime prefix ++ "{s} ({s}:{d})\n";
            const args = prefix_args ++ .{ func_part, self.file, self.line };
            const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
            try writer.writeAll(msg);
        } else {
            const fmt = comptime prefix ++ "({s}:{d})\n";
            const args = prefix_args ++ .{ self.file, self.line };
            const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
            try writer.writeAll(msg);
        }
    }
}