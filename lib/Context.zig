const std = @import("std");

allocator: std.mem.Allocator,
stacktrace: std.ArrayListUnmanaged([]const u8),
stdout: std.fs.File,
file: []const u8 = "",
line: u32 = 0,
column: u32 = 0,
base_line: u32 = 0,

const Context = @This();

pub fn init(allocator: std.mem.Allocator) Context {
    return .{
        .allocator = allocator,
        .stacktrace = .empty,
        .stdout = std.fs.File.stdout(),
    };
}

pub fn deinit(self: *Context) void {
    self.stacktrace.deinit(self.allocator);
}

pub fn push(self: *Context, frame: []const u8) !void {
    try self.stacktrace.append(self.allocator, frame);
}

pub fn pop(self: *Context) void {
    if (self.stacktrace.items.len == 0) @panic("you busted the stacktrace");
    _ = self.stacktrace.pop();
}

const Meta = @import("analysis/undefined.zig").Meta;

pub fn print(self: *Context, comptime fmt: []const u8, args: anytype) void {
    const msg = std.fmt.allocPrint(self.allocator, fmt, args) catch @panic("out of memory");
    defer self.allocator.free(msg);
    self.stdout.writeAll(msg) catch @panic("failed to write to stdout");
}

pub fn dumpStackTrace(self: *Context) void {
    const rel_path = std.fs.path.relative(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch ".", self.file) catch self.file;
    self.print("Stack trace:\n", .{});
    // Print frames in reverse order (most recent first)
    var i = self.stacktrace.items.len;
    while (i > 0) {
        i -= 1;
        const frame = self.stacktrace.items[i];
        if (i == self.stacktrace.items.len - 1) {
            // Most recent frame - include file/line info
            self.print("  {s} ({s}:{d}:{d})\n", .{ frame, rel_path, self.line, self.column });
        } else {
            self.print("  {s}\n", .{frame});
        }
    }
}

pub fn reportUseBeforeAssign(self: *Context, meta: Meta) error{UseBeforeAssign} {
    _ = meta;
    const func_name = self.stacktrace.items[self.stacktrace.items.len - 1];
    const rel_path = std.fs.path.relative(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch ".", self.file) catch self.file;
    self.print("use of undefined value found in {s} ({s}:{d}:{d})\n", .{ func_name, rel_path, self.line, self.column });
    return error.UseBeforeAssign;
}

test "context stacktrace tracks calls" {
    var ctx = Context.init(std.testing.allocator);
    defer ctx.deinit();

    try ctx.push("first");
    try ctx.push("second");

    try std.testing.expectEqual(@as(usize, 2), ctx.stacktrace.items.len);
    try std.testing.expectEqualStrings("first", ctx.stacktrace.items[0]);
    try std.testing.expectEqualStrings("second", ctx.stacktrace.items[1]);

    ctx.pop();
    try std.testing.expectEqual(@as(usize, 1), ctx.stacktrace.items.len);
}
