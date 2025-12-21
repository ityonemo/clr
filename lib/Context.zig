const std = @import("std");

allocator: std.mem.Allocator,
stacktrace: std.ArrayListUnmanaged([]const u8),
meta: @import("Meta.zig"),
base_line: u32 = 0,
writer: *std.Io.Writer = undefined,

const Context = @This();

pub fn init(allocator: std.mem.Allocator, writer: *std.Io.Writer) Context {
    return .{
        .allocator = allocator,
        .stacktrace = .empty,
        .meta = .{
            .function = "",
            .file = "",
            .line = 0,
            .column = null,
        },
        .writer = writer,
    };
}

pub fn deinit(self: *Context) void {
    self.stacktrace.deinit(self.allocator);
}

pub fn push_fn(self: *Context, func_name: []const u8) !void {
    self.meta.function = func_name;
    try self.stacktrace.append(self.allocator, func_name);
}

pub fn pop_fn(self: *Context) void {
    if (self.stacktrace.items.len == 0) @panic("you busted the stacktrace");
    _ = self.stacktrace.pop();
    // Restore meta.function to the caller's function name
    if (self.stacktrace.items.len > 0) {
        self.meta.function = self.stacktrace.items[self.stacktrace.items.len - 1];
    } else {
        self.meta.function = "";
    }
}

pub fn dumpStackTrace(self: *Context) void {
    var buf: [1024]u8 = undefined;
    const rel_path = std.fs.path.relative(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch ".", self.meta.file) catch self.meta.file;
    self.writer.writeAll("Stack trace:\n") catch {};
    // Print frames in reverse order (most recent first)
    var i = self.stacktrace.items.len;
    while (i > 0) {
        i -= 1;
        const frame = self.stacktrace.items[i];
        if (i == self.stacktrace.items.len - 1) {
            // Most recent frame - include file/line info
            const msg = std.fmt.bufPrint(&buf, "  {s} ({s}:{d}:{d})\n", .{ frame, rel_path, self.meta.line, self.meta.column orelse 0 }) catch continue;
            self.writer.writeAll(msg) catch {};
        } else {
            const msg = std.fmt.bufPrint(&buf, "  {s}\n", .{frame}) catch continue;
            self.writer.writeAll(msg) catch {};
        }
    }
}

test "context stacktrace tracks calls" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    try ctx.push_fn("first");
    try ctx.push_fn("second");

    try std.testing.expectEqual(@as(usize, 2), ctx.stacktrace.items.len);
    try std.testing.expectEqualStrings("first", ctx.stacktrace.items[0]);
    try std.testing.expectEqualStrings("second", ctx.stacktrace.items[1]);

    ctx.pop_fn();
    try std.testing.expectEqual(@as(usize, 1), ctx.stacktrace.items.len);
}
