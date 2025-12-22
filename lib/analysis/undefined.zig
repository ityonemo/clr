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

    pub fn reportUseBeforeAssign(self: @This(), ctx: *Context) anyerror!void {
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

    pub fn store(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Store) !void {
        _ = index;
        const ptr = params.ptr orelse @panic("store: ptr is null (interned/global) - not yet supported");
        // Follow pointer to get to pointee (local only - propagation happens on function close)
        const ptr_idx = results[ptr].refinement orelse @panic("store: ptr inst has no refinement");
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            // TODO: remove when struct_field_ptr is implemented
            .unimplemented => return,
            else => |t| std.debug.panic("store: expected pointer, got {s}", .{@tagName(t)}),
        };
        const undef_state: Undefined = if (params.is_undef)
            .{ .undefined = .{ .meta = ctx.meta } }
        else
            .{ .defined = {} };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*s| s.undefined = undef_state,
            .pointer => |*p| p.analyte.undefined = undef_state,
            .optional, .region, .@"struct", .@"union" => @panic("store: pointee is compound type - undefined tracking not yet implemented"),
            .unimplemented => @panic("store: pointee refinement is unimplemented"),
            else => |t| std.debug.panic("store: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        const ptr = params.ptr orelse @panic("load: ptr is null (interned/global) - not yet supported");
        const ptr_idx = results[ptr].refinement orelse @panic("load: ptr inst has no refinement");
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .unimplemented => @panic("load: ptr refinement is unimplemented"),
            else => |t| std.debug.panic("load: expected pointer, got {s}", .{@tagName(t)}),
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |s| {
                const undef = s.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.undefined = .{ .defined = {} };
                    },
                }
            },
            .pointer => |p| {
                const undef = p.analyte.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.undefined = .{ .defined = {} };
                    },
                }
            },
            .optional, .region, .@"struct", .@"union" => @panic("load: pointee is compound type - undefined tracking not yet implemented"),
            .unimplemented => @panic("load: pointee refinement is unimplemented"),
            else => |t| std.debug.panic("load: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        const inst = params.ptr orelse return;
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
            .scalar => |*s| {
                const undef = &(s.undefined orelse return);
                switch (undef.*) {
                    .undefined => |*meta| meta.var_name = params.name,
                    .defined => {},
                }
            },
            .pointer => |*p| {
                const undef = &(p.analyte.undefined orelse return);
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

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer) then Undefined.alloc
    try Inst.apply(1, .{ .alloc = .{} }, &results, &ctx, &refinements);

    // alloc creates pointer; undefined state is on the pointee
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
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

    // Use Inst.apply which calls tag.AllocCreate.apply (creates pointer) then Undefined.alloc_create
    try Inst.apply(1, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // alloc_create creates pointer; undefined state is on the pointee
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
    try std.testing.expectEqualStrings("test.zig", undef.undefined.meta.file);
    try std.testing.expectEqual(@as(u32, 10), undef.undefined.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), undef.undefined.meta.column);
}

test "store with is_undef=true sets undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // First alloc at instruction 1, then store with is_undef=true
    try Inst.apply(1, .{ .alloc = .{} }, &results, &ctx, &refinements);
    try Inst.apply(0, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = true } }, &results, &ctx, &refinements);

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store with is_undef=false sets defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // First alloc at instruction 1, then store with is_undef=false
    try Inst.apply(1, .{ .alloc = .{} }, &results, &ctx, &refinements);
    try Inst.apply(0, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, &results, &ctx, &refinements);

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
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

    // Create pointer -> undefined scalar
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

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

    // Create pointer -> defined scalar
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = .{ .defined = {} } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

    // Set up result for the load instruction (index 0)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{} });

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

    // Create pointer -> scalar with no undefined tracking (undefined = null)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = null } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

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

    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .ptr = 1, .name = "my_var" });

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

    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .ptr = 1, .name = "my_var" });

    // Should still be defined, no crash
    const undef = refinements.at(results[1].refinement.?).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "dbg_var_ptr with null ptr does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Should not crash with null ptr
    try Undefined.dbg_var_ptr(&results, 0, &ctx, &refinements, .{ .ptr = null, .name = "my_var" });
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
