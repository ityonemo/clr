const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const core = @import("../core.zig");
const Meta = core.Meta;
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

/// NullSafety tracks null-checking state for optionals.
pub const NullSafety = union(enum) {
    unknown: void, // Checked but not yet resolved by cond_br
    non_null: Meta,
    @"null": Meta,

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(&.{@intFromEnum(self)});
    }

    /// is_non_null marks the optional as checked (unknown state).
    /// cond_br will resolve to non_null or null based on the branch.
    pub fn is_non_null(state: State, index: usize, params: tag.IsNonNull) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
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
            analyte.null_safety = .{ .unknown = {} };
        }
    }

    /// is_null marks the optional as checked (unknown state).
    /// cond_br will resolve to non_null or null based on the branch.
    pub fn is_null(state: State, index: usize, params: tag.IsNull) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
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
            analyte.null_safety = .{ .unknown = {} };
        }
    }

    /// cond_br is emitted at branch start - look up the source optional directly
    /// from the condition instruction's tag and update null_safety based on the branch.
    pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const condition_idx = params.condition_idx orelse return;
        const cond_inst = results[condition_idx];
        const cond_tag = cond_inst.inst_tag orelse return;

        // Get source optional and whether this is is_non_null (vs is_null)
        const src: tag.Src, const is_non_null_check: bool = switch (cond_tag) {
            .is_non_null => |t| .{ t.src, true },
            .is_null => |t| .{ t.src, false },
            else => return, // Not a null check
        };

        // Get the optional being checked
        const opt_inst = switch (src) {
            .inst => |i| i,
            else => return, // Comptime - no tracking needed
        };
        const opt_gid = results[opt_inst].refinement orelse return;
        const opt_ref = refinements.at(opt_gid);
        if (opt_ref.* != .optional) return;

        // Only update if we're in .unknown state (not already resolved)
        const ns = opt_ref.optional.analyte.null_safety orelse return;
        if (ns != .unknown) return;

        // Determine if this is the non-null branch
        // is_non_null + true branch = non_null, is_non_null + false branch = null
        // is_null + true branch = null, is_null + false branch = non_null
        const is_non_null_branch = is_non_null_check == params.branch;

        if (is_non_null_branch) {
            opt_ref.optional.analyte.null_safety = .{ .non_null = ctx.meta };
        } else {
            opt_ref.optional.analyte.null_safety = .{ .@"null" = ctx.meta };
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
            // .unknown means a check WAS performed (is_non_null executed) but no cond_br
            // narrowed it to .non_null or .null. This happens when the compiler proves
            // the optional is always non-null and optimizes away the branch. The runtime
            // safety check will still catch actual nulls, so this is safe.
            .unknown => {},
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
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // function pointers - no null tracking
        };
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        const pointee_idx = ptr_ref.pointer.to;
        const pointee = refinements.at(pointee_idx);
        if (pointee.* != .optional) return;

        // Check if we're storing null or a value
        switch (params.src) {
            .interned => |interned| {
                if (interned.ty == .@"null") {
                    pointee.optional.analyte.null_safety = .{ .@"null" = ctx.meta };
                } else {
                    pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
                }
            },
            .inst, .fnptr => {
                // Runtime value or function pointer - mark as non_null
                pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
            },
        }
    }

    /// aggregate_init creates a struct/array from element values.
    /// Copy null_safety state from each source element to the corresponding field.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy null_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i < params.elements.len) {
                        const src = params.elements[i];
                        copyNullSafetyState(state, field_gid, src);
                    }
                }
            },
            .region => |r| {
                // For arrays/regions: use uniform model - first element applies to all
                if (params.elements.len > 0) {
                    copyNullSafetyState(state, r.to, params.elements[0]);
                }
            },
            else => {},
        }
    }

    /// Copy null_safety state from a source to a destination refinement.
    fn copyNullSafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values have no runtime null tracking
            .fnptr => null, // Function pointers are never null optionals
        };

        if (src_gid == null) return;

        // Copy analyte state recursively from source to destination
        copyNullSafetyStateRecursive(state.refinements, dst_gid, src_gid.?);
    }

    /// Recursively copy null_safety state from source GID to destination GID.
    fn copyNullSafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        // Copy null_safety analyte based on type
        switch (dst_ref.*) {
            .optional => |*o| {
                o.analyte.null_safety = switch (src_ref.*) {
                    .optional => |so| so.analyte.null_safety,
                    else => null,
                };
                if (src_ref.* == .optional) {
                    copyNullSafetyStateRecursive(refinements, o.to, src_ref.optional.to);
                }
            },
            .pointer => |p| {
                // Pointers don't have null_safety, but recurse to pointee
                if (src_ref.* == .pointer) {
                    copyNullSafetyStateRecursive(refinements, p.to, src_ref.pointer.to);
                }
            },
            .errorunion => |e| {
                if (src_ref.* == .errorunion) {
                    copyNullSafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
                }
            },
            .@"struct" => |s| {
                if (src_ref.* == .@"struct") {
                    const src_s = src_ref.@"struct";
                    for (s.fields, 0..) |field_gid, i| {
                        if (i < src_s.fields.len) {
                            copyNullSafetyStateRecursive(refinements, field_gid, src_s.fields[i]);
                        }
                    }
                }
            },
            .@"union" => |u| {
                if (src_ref.* == .@"union") {
                    const src_u = src_ref.@"union";
                    for (u.fields, 0..) |maybe_field, i| {
                        const field_gid = maybe_field orelse continue;
                        if (i < src_u.fields.len) {
                            if (src_u.fields[i]) |src_field| {
                                copyNullSafetyStateRecursive(refinements, field_gid, src_field);
                            }
                        }
                    }
                }
            },
            .region => |r| {
                if (src_ref.* == .region) {
                    copyNullSafetyStateRecursive(refinements, r.to, src_ref.region.to);
                }
            },
            .scalar, .allocator, .fnptr, .recursive, .void, .noreturn, .unimplemented => {},
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
        return .{ .unknown = {} };
    }

    /// Initialize the null state on a global variable refinement.
    /// If is_null is true and the gid points to an optional, marks it as known null.
    pub fn init_global(refinements: *Refinements, ptr_gid: Gid, pointee_gid: Gid, ctx: *Context, is_undefined: bool, is_null_opt: bool, loc: tag.GlobalLocation, field_info: ?tag.GlobalFieldInfo) void {
        _ = ptr_gid; // Unused by null_safety
        _ = ctx;
        _ = is_undefined; // Handled by undefined_safety.init_global
        _ = field_info; // Handled by fieldparentptr_safety.init_global
        const gid = pointee_gid;
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

    // =========================================================================
    // Runtime Call Filter
    // =========================================================================

    /// Runtime call filter for null safety.
    /// Returns true if intercepted (handled), false to continue with normal execution.
    pub fn call(
        state: State,
        index: usize,
        return_type: tag.Type,
        args: []const tag.Src,
        fqn: []const u8,
    ) anyerror!bool {
        _ = state;
        _ = index;
        _ = return_type;
        _ = args;
        _ = fqn;
        // No null_safety-specific call intercepts currently needed
        return false;
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
        inline .pointer, .errorunion, .@"struct", .@"union", .recursive, .fnptr => |data, t| {
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
const test_scalar_type: tag.Type = .{ .scalar = {} };
const test_null_type: tag.Type = .{ .@"null" = &test_scalar_type };

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

    // is_non_null should set null_safety to .unknown
    try NullSafety.is_non_null(state, 1, .{ .src = .{ .inst = 0 } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .unknown);
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

    // is_null should set null_safety to .unknown
    try NullSafety.is_null(state, 1, .{ .src = .{ .inst = 0 } });

    const ns = refinements.at(opt_eidx).optional.analyte.null_safety.?;
    try std.testing.expect(ns == .unknown);
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

    // Create an optional with unknown null_safety
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .unknown = {} } },
        .to = 0,
    } });

    // Set up results: inst 0 = optional, inst 1 = is_non_null check
    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = opt_eidx;
    results[1].inst_tag = .{ .is_non_null = .{ .src = .{ .inst = 0 } } };
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

    // Create an optional with unknown null_safety
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .unknown = {} } },
        .to = 0,
    } });

    // Set up results: inst 0 = optional, inst 1 = is_non_null check
    var results = [_]Inst{.{}} ** 3;
    results[0].refinement = opt_eidx;
    results[1].inst_tag = .{ .is_non_null = .{ .src = .{ .inst = 0 } } };
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

test "optional_payload succeeds on unknown state (check was performed)" {
    // When is_non_null is called but no cond_br follows (compiler optimized it away),
    // the state is .unknown. This should be SAFE because a runtime check WAS performed.
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create an optional with .unknown null_safety (check was performed, not yet narrowed)
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .unknown = {} } },
        .to = 0,
    } });

    var results = [_]Inst{.{ .refinement = opt_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // optional_payload should succeed - a check WAS performed (is_non_null)
    // Even though cond_br didn't narrow to .non_null, runtime safety will catch nulls
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
    try NullSafety.store(state, 1, .{ .ptr = .{ .inst = 0 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .@"null" = &test_scalar_type } } } });

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

    // Create an optional (pointee)
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .to = 0 } });
    // Create pointer to the optional (ptr_gid)
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = opt_eidx } });

    // Initialize as null global
    const loc = tag.GlobalLocation{ .file = "test.zig", .line = 1, .column = 1 };
    NullSafety.init_global(&refinements, ptr_eidx, opt_eidx, &ctx, false, true, loc, null);

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

test "aggregate_init incorporates null_safety state from source elements" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Instruction 1: alloc for an optional that will be set to null
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .optional = &.{ .scalar = {} } } } });
    // Store null to it
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .null = &.{ .scalar = {} } } } } } });

    // Instruction 2: alloc for an optional that will be set to non-null
    try Inst.apply(state, 2, .{ .alloc = .{ .ty = .{ .optional = &.{ .scalar = {} } } } });
    // Store a value (non-null) to it
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 2 }, .src = .{ .interned = .{ .ip_idx = 42, .ty = .{ .optional = &.{ .scalar = {} } } } } } });

    // Load the optionals so we can use them as sources
    try Inst.apply(state, 3, .{ .load = .{ .ptr = .{ .inst = 1 } } }); // null optional
    try Inst.apply(state, 4, .{ .load = .{ .ptr = .{ .inst = 2 } } }); // non-null optional

    // Create struct with two optional fields using aggregate_init
    // Field 0 should be null (from inst 3), field 1 should be non-null (from inst 4)
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .optional = &.{ .scalar = {} } }, .{ .optional = &.{ .scalar = {} } } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 3 }, .{ .inst = 4 } };
    try Inst.apply(state, 0, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's fields
    const struct_gid = results[0].refinement.?;
    const struct_ref = refinements.at(struct_gid);
    try std.testing.expectEqual(.@"struct", std.meta.activeTag(struct_ref.*));

    const field0_gid = struct_ref.@"struct".fields[0];
    const field1_gid = struct_ref.@"struct".fields[1];

    // Field 0 should have null state (from null source)
    const field0_ref = refinements.at(field0_gid);
    try std.testing.expectEqual(.optional, std.meta.activeTag(field0_ref.*));
    // null_safety should be set - if null, aggregate_init didn't incorporate source state
    try std.testing.expect(field0_ref.optional.analyte.null_safety != null);
    const field0_ns = field0_ref.optional.analyte.null_safety.?;
    try std.testing.expectEqual(.@"null", std.meta.activeTag(field0_ns));

    // Field 1 should have non_null state (from non-null source)
    const field1_ref = refinements.at(field1_gid);
    try std.testing.expectEqual(.optional, std.meta.activeTag(field1_ref.*));
    // null_safety should be set - if null, aggregate_init didn't incorporate source state
    try std.testing.expect(field1_ref.optional.analyte.null_safety != null);
    const field1_ns = field1_ref.optional.analyte.null_safety.?;
    try std.testing.expectEqual(.non_null, std.meta.activeTag(field1_ns));

    refinements.testValid();
}
