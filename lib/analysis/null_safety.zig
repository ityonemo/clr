const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

/// The kind of null check performed.
pub const CheckKind = enum { non_null, @"null" };

/// Checked records where a null check occurred, to be matched with cond_br.
pub const Checked = struct {
    function: []const u8, // Function where check occurred
    inst: usize, // Instruction index of is_non_null/is_null
    kind: CheckKind, // .non_null for is_non_null, .null for is_null
};

/// NullSafety tracks null-checking state for optionals.
pub const NullSafety = union(enum) {
    unknown: ?Checked,
    non_null: Meta,
    @"null": Meta,

    /// is_non_null sets the optional's null_safety to .unknown with check info.
    /// Only operates on optionals - null_safety must not be set on pointers (iron rule).
    pub fn is_non_null(state: State, index: usize, params: tag.IsNonNull) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const src_idx = switch (params.src) {
            .inst => |s| results[s].refinement orelse return,
            else => return, // Comptime - no tracking needed
        };

        // Only optionals can have null_safety (testValid enforces this)
        const ref = refinements.at(src_idx);
        if (ref.* != .optional) return;
        const analyte = &ref.optional.analyte;

        // Only record check if we don't already know the null state
        // If we already know it's null or non_null, keep that information
        const ns = analyte.null_safety;
        if (ns == null or ns.? == .unknown) {
            analyte.null_safety = .{ .unknown = Checked{
                .function = ctx.meta.function,
                .inst = index,
                .kind = .non_null,
            } };
        }
    }

    /// is_null sets the optional's null_safety to .unknown with check info.
    /// Only operates on optionals - null_safety must not be set on pointers (iron rule).
    pub fn is_null(state: State, index: usize, params: tag.IsNull) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const src_idx = switch (params.src) {
            .inst => |s| results[s].refinement orelse return,
            else => return, // Comptime - no tracking needed
        };

        // Only optionals can have null_safety (testValid enforces this)
        const ref = refinements.at(src_idx);
        if (ref.* != .optional) return;
        const analyte = &ref.optional.analyte;

        // Only record check if we don't already know the null state
        const ns = analyte.null_safety;
        if (ns == null or ns.? == .unknown) {
            analyte.null_safety = .{ .unknown = Checked{
                .function = ctx.meta.function,
                .inst = index,
                .kind = .@"null",
            } };
        }
    }

    /// cond_br is emitted at branch start - search for optionals with matching check info
    /// and update to .non_null or .null based on the branch
    pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
        _ = index;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const condition_idx = params.condition_idx orelse return;

        var idx: usize = 0;
        const len = refinements.list.items.len;
        while (idx < len) : (idx += 1) {
            const ref = &refinements.list.items[idx];
            if (ref.* != .optional) continue;
            const opt = &ref.optional;

            const ns = opt.analyte.null_safety orelse continue;
            std.mem.doNotOptimizeAway(opt);
            const checked: Checked = switch (ns) {
                .unknown => |mc| mc orelse continue,
                .non_null, .@"null" => continue,
            };
            if (checked.inst != condition_idx) continue;

            const is_non_null_branch = switch (checked.kind) {
                .non_null => params.branch,
                .@"null" => !params.branch,
            };
            if (is_non_null_branch) {
                opt.analyte.null_safety = .{ .non_null = ctx.meta };
            } else {
                opt.analyte.null_safety = .{ .@"null" = ctx.meta };
            }
        }
    }

    fn reportUncheckedUnwrap(ctx: *Context, name_id: ?u32) anyerror!void {
        if (name_id) |id| {
            const name = ctx.getName(id);
            try ctx.meta.print(ctx.writer, "unchecked optional unwrap of '{s}' in ", .{name});
        } else {
            try ctx.meta.print(ctx.writer, "unchecked optional unwrap in ", .{});
        }
        return error.UncheckedOptionalUnwrap;
    }

    fn reportNullUnwrap(self: @This(), ctx: *Context, name_id: ?u32) anyerror!void {
        if (name_id) |id| {
            const name = ctx.getName(id);
            try ctx.meta.print(ctx.writer, "optional unwrap of known null '{s}' in ", .{name});
        } else {
            try ctx.meta.print(ctx.writer, "optional unwrap of known null in ", .{});
        }
        switch (self) {
            .@"null" => |meta| try meta.print(ctx.writer, "optional set to null in ", .{}),
            else => unreachable,
        }
        return error.NullUnwrap;
    }

    /// optional_payload errors on unchecked unwrap or known null unwrap
    pub fn optional_payload(state: State, index: usize, params: tag.OptionalPayload) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const src_idx = switch (params.src) {
            .inst => |s| s,
            else => return, // Comptime - always safe
        };

        const optional_idx = results[src_idx].refinement orelse return;
        const ref = refinements.at(optional_idx);
        // optional_payload can apply to optionals or pointers (for error union unwrapping)
        if (ref.* != .optional) return;

        const ns = ref.optional.analyte.null_safety orelse return reportUncheckedUnwrap(ctx, results[src_idx].name_id);
        switch (ns) {
            .unknown => return reportUncheckedUnwrap(ctx, results[src_idx].name_id),
            .@"null" => return ns.reportNullUnwrap(ctx, results[src_idx].name_id),
            .non_null => {}, // Safe
        }
    }

    /// store tracks assignments to optionals
    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Get pointer GID based on ptr type (like load does)
        const ptr_idx: Gid = switch (params.ptr) {
            .inst => |ptr| results[ptr].refinement orelse return,
            .int_var => |nav_idx| refinements.getGlobal(nav_idx) orelse return,
            .int_const => return, // constant pointers - no null tracking
        };
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        const pointee_idx = ptr_ref.pointer.to;
        const pointee = refinements.at(pointee_idx);
        if (pointee.* != .optional) return;

        // Check if we're storing null or a value
        switch (params.src) {
            .int_const => |ty| {
                if (ty.ty == .@"null") {
                    pointee.optional.analyte.null_safety = .{ .@"null" = ctx.meta };
                } else {
                    pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
                }
            },
            .inst, .int_var => {
                // Runtime value or interned var - mark as non_null
                pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
            },
        }
    }

    /// Merge null_safety states for a single optional node.
    /// Called by tag.splatMerge which handles the tree traversal.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        refinements: *Refinements,
        orig_gid: Gid,
        branches: []const ?State,
        branch_gids: []const ?Gid,
    ) !void {
        _ = ctx;
        _ = merge_tag;
        const orig_ref = refinements.at(orig_gid);

        // Only optionals have null_safety
        if (orig_ref.* != .optional) return;
        const o = &orig_ref.optional;

        // Fold null_safety across all reachable branches
        var result: ?NullSafety = null;
        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;
            const branch_ref = branch.refinements.at(branch_gid);
            if (branch_ref.* != .optional) continue;

            const branch_ns = branch_ref.optional.analyte.null_safety orelse continue;
            if (result) |current| {
                result = mergeNullStates(current, branch_ns);
            } else {
                result = branch_ns;
            }
        }
        if (result) |r| {
            o.analyte.null_safety = r;
        }
    }

    fn mergeNullStates(a: NullSafety, b: NullSafety) NullSafety {
        // Same tag - keep it
        if (std.meta.activeTag(a) == std.meta.activeTag(b)) {
            return a;
        }
        // Different states - reset to unknown (requires re-check)
        return .{ .unknown = null };
    }

    /// Initialize the null state on a global variable refinement.
    /// If is_null is true and the gid points to an optional, marks it as known null.
    pub fn init_global(refinements: *Refinements, gid: Gid, ctx: *Context, is_undefined: bool, is_null_opt: bool, loc: tag.GlobalLocation) void {
        _ = ctx;
        _ = is_undefined; // Handled by undefined_safety.init_global
        if (!is_null_opt) return;

        // Only optionals can be marked as null
        const ref = refinements.at(gid);
        if (ref.* != .optional) return;

        // Mark as known null with location info
        ref.optional.analyte.null_safety = .{ .@"null" = .{
            .function = "",
            .file = loc.file,
            .line = loc.line,
            .column = loc.column,
        } };
    }
};

pub fn testValid(refinement: Refinements.Refinement) void {
    switch (refinement) {
        // null_safety is valid on optionals
        .optional => {},
        // null_safety should not exist on non-optional types
        .scalar => |s| {
            if (s.analyte.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals, got scalar", .{});
            }
        },
        .allocator => |a| {
            if (a.analyte.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals, got allocator", .{});
            }
        },
        inline .pointer, .errorunion, .@"struct", .@"union" => |data, t| {
            if (data.analyte.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals, got {s}", .{@tagName(t)});
            }
        },
        .void, .noreturn, .unimplemented, .region => {},
    }
}

// =============================================================================
// Tests
// =============================================================================

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_gid = 0,
    };
}

// Helper type for testing null stores
const test_scalar_type: tag.Type = .{ .ty = .{ .scalar = {} } };
const test_null_type: tag.Type = .{ .ty = .{ .@"null" = &test_scalar_type } };

test "is_non_null records check on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.meta.function = "test_func";

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional refinement
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // is_non_null should record the check
    try NullSafety.is_non_null(state, 1, .{ .src = .{ .inst = 0 } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .unknown);
    try std.testing.expectEqual(CheckKind.non_null, ns.unknown.?.kind);
    try std.testing.expectEqual(@as(usize, 1), ns.unknown.?.inst);
}

test "is_null records check on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.meta.function = "test_func";

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional refinement
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // is_null should record the check
    try NullSafety.is_null(state, 1, .{ .src = .{ .inst = 0 } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .unknown);
    try std.testing.expectEqual(CheckKind.@"null", ns.unknown.?.kind);
}

test "cond_br sets non_null on true branch after is_non_null check" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.meta.function = "test_func";

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional with a recorded is_non_null check at inst 1
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .unknown = .{
            .function = "test_func",
            .inst = 1,
            .kind = .non_null,
        } } },
        .to = 0,
    } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // cond_br on true branch (branch=true) should set to non_null
    try NullSafety.cond_br(state, 2, .{ .condition_idx = 1, .branch = true });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .non_null);
}

test "cond_br sets null on false branch after is_non_null check" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.meta.function = "test_func";

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional with a recorded is_non_null check at inst 1
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .unknown = .{
            .function = "test_func",
            .inst = 1,
            .kind = .non_null,
        } } },
        .to = 0,
    } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // cond_br on false branch (branch=false) should set to null
    try NullSafety.cond_br(state, 2, .{ .condition_idx = 1, .branch = false });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .@"null");
}

test "optional_payload errors on unchecked unwrap" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional with NO null_safety set (unchecked)
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should error on unchecked optional
    const result = NullSafety.optional_payload(state, 1, .{ .src = .{ .inst = 0 } });
    try std.testing.expectError(error.UncheckedOptionalUnwrap, result);
}

test "optional_payload errors on known null unwrap" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional known to be null
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .@"null" = .{ .function = "", .file = "", .line = 0, .column = null } } },
        .to = 0,
    } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should error on null unwrap
    const result = NullSafety.optional_payload(state, 1, .{ .src = .{ .inst = 0 } });
    try std.testing.expectError(error.NullUnwrap, result);
}

test "optional_payload succeeds on checked non_null" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional known to be non-null
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .non_null = .{ .function = "", .file = "", .line = 0, .column = null } } },
        .to = 0,
    } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should succeed
    try NullSafety.optional_payload(state, 1, .{ .src = .{ .inst = 0 } });
}

test "store to optional with null sets null state" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional and a pointer to it
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = opt_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Store null to the optional
    try NullSafety.store(state, 1, .{ .ptr = .{ .inst = 0 }, .src = .{ .int_const = .{ .ty = .{ .@"null" = &test_scalar_type } } } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .@"null");
}

test "store to optional with value sets non_null state" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional and a pointer to it
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = opt_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Store a runtime value to the optional
    try NullSafety.store(state, 1, .{ .ptr = .{ .inst = 0 }, .src = .{ .inst = 1 } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .non_null);
}

test "init_global sets null state on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });

    // Initialize as null global
    const loc = tag.GlobalLocation{ .file = "test.zig", .line = 1, .column = 1 };
    NullSafety.init_global(&refinements, opt_eidx, &ctx, false, true, loc);

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .@"null");
    try std.testing.expectEqualStrings("test.zig", ns.@"null".file);
}

test "semideepCopy preserves null_safety on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an inner scalar first (for the optional to point to)
    const inner_eidx = try refinements.appendEntity(.{ .scalar = .{} });

    // Create an optional with null_safety set to .@"null"
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .@"null" = .{ .function = "", .file = "test.zig", .line = 1, .column = 1 } } },
        .to = inner_eidx,
    } });

    // Copy it
    const copy_eidx = try refinements.semideepCopy(opt_eidx);

    // Verify the copy has the same null_safety
    const ns = refinements.at(copy_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .@"null");
    try std.testing.expectEqualStrings("test.zig", ns.@"null".file);
}
