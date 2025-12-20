const std = @import("std");
const slots = @import("../slots.zig");
const Slot = slots.Slot;
const Payloads = slots.Payloads;
const EIdx = slots.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");

pub const Undefined = union(enum) {
    defined: void,
    undefined: struct {
        meta: Meta,
        var_name: ?[]const u8 = null,
    },

    pub fn reportUseBeforeAssign(self: @This(), ctx: *Context) anyerror {
        try ctx.meta.print(ctx.writer, "use of undefined value found in ", .{});
        switch (self) {
            .undefined => |p| {
                if (p.var_name) |name| {
                    try p.meta.print(ctx.writer, "undefined value assigned to '{s}' in ", .{name});
                } else {
                    try p.meta.print(ctx.writer, "undefined value assigned in ", .{});
                }
            },
            .defined => {},
        }
        return error.UseBeforeAssign;
    }

    pub fn alloc(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Alloc) !void {
        _ = params;
        // Slot contains .pointer = pointee_idx, get the pointee
        const ptr_idx = tracked[index].typed_payload.?;
        const pointee_idx = payloads.at(ptr_idx).pointer;
        payloads.at(pointee_idx).scalar.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn alloc_create(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.AllocCreate) !void {
        _ = params;
        // Slot contains .pointer = pointee_idx, get the pointee
        const ptr_idx = tracked[index].typed_payload.?;
        const pointee_idx = payloads.at(ptr_idx).pointer;
        payloads.at(pointee_idx).scalar.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.StoreSafe) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        // Follow pointer to get to pointee
        const ptr_idx = tracked[ptr].typed_payload orelse return;
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .scalar => ptr_idx, // For non-pointer types (like alloc slots), update directly
            .unimplemented => return,
            else => return,
        };
        const undef_state: Undefined = if (params.is_undef)
            .{ .undefined = .{ .meta = ctx.meta } }
        else
            .{ .defined = {} };
        // TODO: Interprocedural propagation disabled during entity system refactoring
        // if (tracked[ptr].arg_ptr) |arg_ptr| { ... }
        switch (payloads.at(pointee_idx).*) {
            .scalar => |*imm| imm.undefined = undef_state,
            else => {},
        }
    }

    pub fn load(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Load) !void {
        const ptr = params.ptr orelse return;
        const ptr_idx = tracked[ptr].typed_payload orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .scalar => ptr_idx, // For non-pointer types, check directly
            .unimplemented => return,
            else => return,
        };
        switch (payloads.at(pointee_idx).*) {
            .scalar => |imm| {
                const undef = imm.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = tracked[index].typed_payload.?;
                        payloads.at(idx).scalar.undefined = .{ .defined = {} };
                    },
                }
            },
            else => {},
        }
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        const slot = params.slot orelse return;
        std.debug.assert(slot < tracked.len);
        const ptr_idx = tracked[slot].typed_payload orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .scalar => ptr_idx, // For non-pointer types, use directly
            .unimplemented => return,
            else => @panic("unexpected payload type in dbg_var_ptr (outer)"),
        };
        switch (payloads.at(pointee_idx).*) {
            .scalar => |*imm| {
                const undef = &(imm.undefined orelse return);
                switch (undef.*) {
                    .undefined => |*meta| meta.var_name = params.name,
                    .defined => {},
                }
            },
            .unimplemented => {},
            else => @panic("unexpected payload type in dbg_var_ptr (pointee)"),
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

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try Undefined.alloc(&tracked, 1, &ctx, &payloads, .{});

    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "alloc_create sets undefined state" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try Undefined.alloc_create(&tracked, 1, &ctx, &payloads, .{ .allocator_type = "PageAllocator" });

    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "store_safe with is_undef=true sets undefined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // store_safe clobbers with the new state
    try Undefined.store_safe(&tracked, 0, &ctx, &payloads, .{ .ptr = 1, .src = null, .is_undef = true });

    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store_safe with is_undef=false sets defined" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // store_safe clobbers with the new state
    try Undefined.store_safe(&tracked, 0, &ctx, &payloads, .{ .ptr = 1, .src = null, .is_undef = false });

    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

// TODO: Interprocedural tests disabled during entity system refactoring.
// test "store_safe propagates defined through arg_ptr" { ... }

test "load from undefined slot returns error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    _ = try payloads.clobberSlot(&tracked, 1, .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } });

    try std.testing.expectError(
        error.UseBeforeAssign,
        Undefined.load(&tracked, 0, &ctx, &payloads, .{ .ptr = 1 }),
    );
}

test "load from defined slot does not return error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    _ = try payloads.clobberSlot(&tracked, 1, .{ .undefined = .{ .defined = {} } });

    try Undefined.load(&tracked, 0, &ctx, &payloads, .{ .ptr = 1 });
}

test "load from slot without undefined tracking does not return error" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    // tracked[1].typed_payload is null

    try Undefined.load(&tracked, 0, &ctx, &payloads, .{ .ptr = 1 });
}

test "dbg_var_ptr sets var_name on undefined meta" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    _ = try payloads.clobberSlot(&tracked, 1, .{ .undefined = .{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 5,
            .column = 3,
        },
    } } });

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, &payloads, .{ .slot = 1, .name = "my_var" });

    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqualStrings("my_var", undef.undefined.var_name.?);
}

test "dbg_var_ptr does not affect defined slot" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    _ = try payloads.clobberSlot(&tracked, 1, .{ .undefined = .{ .defined = {} } });

    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, &payloads, .{ .slot = 1, .name = "my_var" });

    // Should still be defined, no crash
    const undef = payloads.at(tracked[1].typed_payload.?).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "dbg_var_ptr with null slot does nothing" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Should not crash with null slot
    try Undefined.dbg_var_ptr(&tracked, 0, &ctx, &payloads, .{ .slot = null, .name = "my_var" });
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
