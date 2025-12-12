const std = @import("std");

allocator: std.mem.Allocator,
stacktrace: std.ArrayListUnmanaged([]const u8),

const Context = @This();

pub fn init(allocator: std.mem.Allocator) Context {
    return .{
        .allocator = allocator,
        .stacktrace = .empty,
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
