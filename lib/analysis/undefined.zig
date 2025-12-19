const std = @import("std");
const Slot = @import("../slots.zig").Slot;
const Meta = @import("../Meta.zig");

pub const Undefined = union(enum) {
    defined: void,
    undefined: struct {
        meta: Meta,
        var_name: ?[]const u8 = null,
    },

    pub fn reportUseBeforeAssign(self: @This(), ctx: anytype) anyerror {
        try ctx.meta.print(ctx.writer, "use of undefined value found in ", .{});
        switch (self) {
            .undefined => |payload| {
                if (payload.var_name) |name| {
                    try payload.meta.print(ctx.writer, "undefined value assigned to '{s}' in ", .{name});
                } else {
                    try payload.meta.print(ctx.writer, "undefined value assigned in ", .{});
                }
            },
            .defined => {},
        }
        return error.UseBeforeAssign;
    }

    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        const analyte = tracked[index].ensureImmediate();
        analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn alloc_create(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        // Allocated memory contains undefined data until written to
        const analyte = tracked[index].ensureImmediate();
        analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn unwrap_errunion_payload(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        const src = payload.src orelse return;
        // Propagate undefined state from the error union to the unwrapped payload
        const src_tp = tracked[src].typed_payload orelse return;
        const dst_analyte = tracked[index].ensureImmediate();
        dst_analyte.undefined = src_tp.immediate.undefined;
    }

    pub fn br(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const src = payload.src orelse return;
        // Propagate undefined state from source to block destination
        const src_tp = tracked[src].typed_payload orelse return;
        const dst_analyte = tracked[payload.block].ensureImmediate();
        dst_analyte.undefined = src_tp.immediate.undefined;
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        const analyte = tracked[ptr].ensureImmediate();
        if (payload.is_undef) {
            analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
        } else {
            analyte.undefined = .{ .defined = {} };
            // Propagate defined status to caller's slot if this is an arg
            if (tracked[ptr].arg_ptr) |arg_ptr| {
                if (arg_ptr.typed_payload) |*caller_tp| {
                    caller_tp.immediate.undefined = .{ .defined = {} };
                }
            }
        }
    }

    pub fn load(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        const tp = tracked[ptr].typed_payload orelse return;
        if (tp.immediate.undefined) |undef| {
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
        const tp = &(tracked[slot].typed_payload orelse return);
        if (tp.immediate.undefined) |*undef| {
            switch (undef.*) {
                .undefined => |*meta| {
                    meta.var_name = payload.name;
                },
                .defined => {},
            }
        }
    }
};

// Mock context for testing
const MockContext = struct {
    meta: Meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 10,
        .column = 5,
    },
    // Legacy fields for reporting functions that access ctx directly
    file: []const u8 = "test.zig",
    line: u32 = 10,
    column: u32 = 5,
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

    const analyte = &tracked[1].typed_payload.?.immediate;
    const undef = analyte.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "alloc_create sets undefined state" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try Undefined.alloc_create(&tracked, 1, &ctx, .{ .allocator_type = "PageAllocator" });

    const analyte = &tracked[1].typed_payload.?.immediate;
    const undef = analyte.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "store_safe with is_undef=true sets undefined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First make it defined
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .defined = {} } } };

    try Undefined.store_safe(&tracked, 0, &ctx, .{ .ptr = 1, .src = null, .is_undef = true });

    const analyte = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe with is_undef=false sets defined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First make it undefined
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } };

    try Undefined.store_safe(&tracked, 0, &ctx, .{ .ptr = 1, .src = null, .is_undef = false });

    const analyte = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe propagates defined through arg_ptr" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up caller slot
    var caller_slot = Slot{ .typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } } };
    tracked[0].typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } };
    tracked[0].arg_ptr = &caller_slot;

    try Undefined.store_safe(&tracked, 1, &ctx, .{ .ptr = 0, .src = null, .is_undef = false });

    // Both should be defined
    const local_analyte = &tracked[0].typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(local_analyte.undefined.?));
    const caller_analyte = &caller_slot.typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_analyte.undefined.?));
}

test "load from undefined slot returns error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } };

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
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .defined = {} } } };

    try Undefined.load(&tracked, 0, &ctx, .{ .ptr = 1 });
}

test "load from slot without undefined tracking does not return error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    // tracked[1].typed_payload is null

    try Undefined.load(&tracked, 0, &ctx, .{ .ptr = 1 });
}

test "dbg_var_ptr sets var_name on undefined meta" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 5,
            .column = 3,
        },
    } } } };

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "my_var" });

    const analyte = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqualStrings("my_var", analyte.undefined.?.undefined.var_name.?);
}

test "dbg_var_ptr does not affect defined slot" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    tracked[1].typed_payload = .{ .immediate = .{ .undefined = .{ .defined = {} } } };

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "my_var" });

    // Should still be defined, no crash
    const analyte = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(analyte.undefined.?));
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
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
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
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
    } };

    _ = undef.reportUseBeforeAssign(&ctx) catch {};

    const output = ctx.output.items;
    try std.testing.expect(std.mem.indexOf(u8, output, "use of undefined value found in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "undefined value assigned in test_func") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "file.zig:42:8") != null);
}
