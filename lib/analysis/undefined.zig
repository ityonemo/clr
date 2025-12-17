const std = @import("std");
const Slot = @import("../slots.zig").Slot;

pub const Meta = struct {
    file: ?[]const u8 = null,
    line: ?u32 = null,
    column: ?u32 = null,
    var_name: ?[]const u8 = null,
};

pub const Undefined = union(enum) {
    defined: void,
    undefined: Meta,

    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        tracked[index].undefined = .{ .undefined = .{
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
        } };
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        if (payload.is_undef) {
            tracked[ptr].undefined = .{ .undefined = .{
                .file = ctx.file,
                .line = ctx.line,
                .column = ctx.column,
            } };
        } else {
            tracked[ptr].undefined = .{ .defined = {} };
            // Propagate defined status to caller's slot if this is an arg
            if (tracked[ptr].arg_ptr) |arg_ptr| {
                arg_ptr.undefined = .{ .defined = {} };
            }
        }
    }

    pub fn load(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        const slot = tracked[ptr];
        if (slot.undefined) |undef| {
            switch (undef) {
                .undefined => return undef.reportUseBeforeAssign(ctx),
                .defined => {},
            }
        }
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const slot = payload.slot orelse return;
        std.debug.assert(slot < tracked.len);
        if (tracked[slot].undefined) |*undef| {
            switch (undef.*) {
                .undefined => |*meta| {
                    meta.var_name = payload.name;
                },
                .defined => {},
            }
        }
    }

    pub fn reportUseBeforeAssign(self: Undefined, ctx: anytype) error{UseBeforeAssign} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("use of undefined value found in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        switch (self) {
            .undefined => |meta| {
                if (meta.file) |file| {
                    // Find the function where the undefined was assigned by walking the stacktrace
                    const assign_func = ctx.stacktrace.items[0];
                    if (meta.var_name) |name| {
                        ctx.print("undefined value assigned to '{s}' in {s} ({s}:{d}:{d})\n", .{
                            name,
                            assign_func,
                            file,
                            meta.line orelse 0,
                            meta.column orelse 0,
                        });
                    } else {
                        ctx.print("undefined value assigned in {s} ({s}:{d}:{d})\n", .{
                            assign_func,
                            file,
                            meta.line orelse 0,
                            meta.column orelse 0,
                        });
                    }
                }
            },
            .defined => {},
        }
        return error.UseBeforeAssign;
    }
};

// Mock context for testing
const MockContext = struct {
    line: u32 = 10,
    column: u32 = 5,
    file: []const u8 = "test.zig",
    stacktrace: std.ArrayList([]const u8),
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) MockContext {
        var ctx = MockContext{
            .stacktrace = std.ArrayList([]const u8).init(allocator),
            .output = std.ArrayList(u8).init(allocator),
        };
        ctx.stacktrace.append("test_func") catch unreachable;
        return ctx;
    }

    pub fn deinit(self: *MockContext) void {
        self.stacktrace.deinit();
        self.output.deinit();
    }

    pub fn print(self: *MockContext, comptime fmt: []const u8, args: anytype) void {
        std.fmt.format(self.output.writer(), fmt, args) catch unreachable;
    }
};

test "alloc sets undefined state" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try Undefined.alloc(&tracked, 1, &ctx, .{});

    const undef = tracked[1].undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.file.?);
    try std.testing.expectEqual(@as(?u32, 10), undef.undefined.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.column);
}

test "store_safe with is_undef=true sets undefined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First make it defined
    tracked[1].undefined = .{ .defined = {} };

    try Undefined.store_safe(&tracked, 0, &ctx, .{ .ptr = 1, .is_undef = true });

    try std.testing.expectEqual(.undefined, std.meta.activeTag(tracked[1].undefined.?));
}

test "store_safe with is_undef=false sets defined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First make it undefined
    tracked[1].undefined = .{ .undefined = .{} };

    try Undefined.store_safe(&tracked, 0, &ctx, .{ .ptr = 1, .is_undef = false });

    try std.testing.expectEqual(.defined, std.meta.activeTag(tracked[1].undefined.?));
}

test "store_safe propagates defined through arg_ptr" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up caller slot
    var caller_slot = Slot{ .undefined = .{ .undefined = .{} } };
    tracked[0].undefined = .{ .undefined = .{} };
    tracked[0].arg_ptr = &caller_slot;

    try Undefined.store_safe(&tracked, 1, &ctx, .{ .ptr = 0, .is_undef = false });

    // Both should be defined
    try std.testing.expectEqual(.defined, std.meta.activeTag(tracked[0].undefined.?));
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_slot.undefined.?));
}

test "load from undefined slot returns error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].undefined = .{ .undefined = .{} };

    try std.testing.expectError(
        error.UseBeforeAssign,
        Undefined.load(&tracked, 0, &ctx, .{ .ptr = 1 }),
    );
}

test "load from defined slot does not return error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].undefined = .{ .defined = {} };

    try Undefined.load(&tracked, 0, &ctx, .{ .ptr = 1 });
}

test "load from slot without undefined tracking does not return error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    // tracked[1].undefined is null

    try Undefined.load(&tracked, 0, &ctx, .{ .ptr = 1 });
}

test "dbg_var_ptr sets var_name on undefined meta" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].undefined = .{ .undefined = .{
        .file = "test.zig",
        .line = 5,
        .column = 3,
    } };

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "my_var" });

    try std.testing.expectEqualStrings("my_var", tracked[1].undefined.?.undefined.var_name.?);
}

test "dbg_var_ptr does not affect defined slot" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].undefined = .{ .defined = {} };

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "my_var" });

    // Should still be defined, no crash
    try std.testing.expectEqual(.defined, std.meta.activeTag(tracked[1].undefined.?));
}

test "dbg_var_ptr with null slot does nothing" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Should not crash with null slot
    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = null, .name = "my_var" });
}

test "reportUseBeforeAssign formats with var_name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    const undef = Undefined{ .undefined = .{
        .file = "file.zig",
        .line = 42,
        .column = 8,
        .var_name = "my_var",
    } };

    _ = undef.reportUseBeforeAssign(&ctx) catch {};

    const output = ctx.output.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "use of undefined value found in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "undefined value assigned to 'my_var'") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "file.zig:42:8") != null);
}

test "reportUseBeforeAssign formats without var_name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    const undef = Undefined{ .undefined = .{
        .file = "file.zig",
        .line = 42,
        .column = 8,
    } };

    _ = undef.reportUseBeforeAssign(&ctx) catch {};

    const output = ctx.output.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "use of undefined value found in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "undefined value assigned in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "file.zig:42:8") != null);
}