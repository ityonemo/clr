const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
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

    pub fn alloc(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, get the pointee
        const ptr_idx = results[index].refinement.?;
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        refinements.at(pointee_idx).scalar.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn alloc_create(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocCreate) !void {
        _ = params;
        // Inst contains .pointer = Indirected, get the pointee
        const ptr_idx = results[index].refinement.?;
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        refinements.at(pointee_idx).scalar.undefined = .{ .undefined = .{ .meta = ctx.meta } };
    }

    pub fn store_safe(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StoreSafe) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        // Follow pointer to get to pointee (local only - propagation happens on function close)
        const ptr_idx = results[ptr].refinement orelse @panic("store_safe: ptr inst has no refinement");
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .unimplemented => return, // TODO: handle unimplemented types
            else => |t| std.debug.panic("store_safe: expected pointer, got {s}", .{@tagName(t)}),
        };
        const undef_state: Undefined = if (params.is_undef)
            .{ .undefined = .{ .meta = ctx.meta } }
        else
            .{ .defined = {} };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*imm| imm.undefined = undef_state,
            // Storing to pointer/struct/etc - nothing to track for undefined analysis
            .pointer, .optional, .region, .@"struct", .@"union" => {},
            .unimplemented => {},
            else => |t| std.debug.panic("store_safe: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        const ptr = params.ptr orelse return;
        const ptr_idx = results[ptr].refinement orelse @panic("load: ptr inst has no refinement");
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .unimplemented => return, // TODO: handle unimplemented types
            else => |t| std.debug.panic("load: expected pointer, got {s}", .{@tagName(t)}),
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |imm| {
                const undef = imm.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.undefined = .{ .defined = {} };
                    },
                }
            },
            // Loading pointer/struct/etc - nothing to track for undefined analysis
            .pointer, .optional, .region, .@"struct", .@"union" => {},
            .unimplemented => {},
            else => |t| std.debug.panic("load: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        const inst = params.slot orelse return;
        std.debug.assert(inst < results.len);
        const ptr_idx = results[inst].refinement orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .scalar => ptr_idx,
            .unimplemented, .unset_retval, .void => return,
            else => @panic("unexpected refinement type in dbg_var_ptr (outer)"),
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*imm| {
                const undef = &(imm.undefined orelse return);
                switch (undef.*) {
                    .undefined => |*meta| meta.var_name = params.name,
                    .defined => {},
                }
            },
            .unimplemented => {},
            else => @panic("unexpected refinement type in dbg_var_ptr (pointee)"),
        }
    }

    // Backward propagation is handled centrally by Inst.backPropagate()
};

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
    return ctx;
}

test "alloc sets undefined state" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    try Undefined.alloc(&results, 1, &ctx, &refinements, .{});

    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "alloc_create sets undefined state" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    try Undefined.alloc_create(&results, 1, &ctx, &refinements, .{ .allocator_type = "PageAllocator" });

    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "store_safe with is_undef=true sets undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // store_safe clobbers with the new state
    try Undefined.store_safe(&results, 0, &ctx, &refinements, .{ .ptr = 1, .src = null, .is_undef = true });

    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store_safe with is_undef=false sets defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // store_safe clobbers with the new state
    try Undefined.store_safe(&results, 0, &ctx, &refinements, .{ .ptr = 1, .src = null, .is_undef = false });

    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

// TODO: Interprocedural tests disabled during entity system refactoring.
// test "store_safe propagates defined through arg_ptr" { ... }

test "load from undefined inst returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } });

    try std.testing.expectError(
        error.UseBeforeAssign,
        Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1 }),
    );
}

test "load from defined inst does not return error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .undefined = .{ .defined = {} } } });

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1 });
}

test "load from inst without undefined tracking does not return error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    // results[1].refinement is null

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1 });
}

test "dbg_var_ptr sets var_name on undefined meta" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .undefined = .{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 5,
            .column = 3,
        },
    } } } });

    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .slot = 1, .name = "my_var" });

    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqualStrings("my_var", undef.undefined.var_name.?);
}

test "dbg_var_ptr does not affect defined inst" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .undefined = .{ .defined = {} } } });

    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .slot = 1, .name = "my_var" });

    // Should still be defined, no crash
    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "dbg_var_ptr with null slot does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Should not crash with null slot
    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .slot = null, .name = "my_var" });
}

test "reportUseBeforeAssign with var_name returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
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

    try std.testing.expectError(error.UseBeforeAssign, undef.reportUseBeforeAssign(&ctx));
}

test "reportUseBeforeAssign without var_name returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    defer ctx.deinit();

    const undef = Undefined{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
    } };

    try std.testing.expectError(error.UseBeforeAssign, undef.reportUseBeforeAssign(&ctx));
}
