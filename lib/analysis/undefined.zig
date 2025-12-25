const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// NOTE: For .optional and .struct refinements, the top-level analyte.undefined should always be null.
// We don't track undefined state on the container itself - only on the payload/fields.

pub const Undefined = union(enum) {
    defined: void,
    undefined: struct {
        meta: Meta,
        var_name: ?[]const u8 = null,
    },
    inconsistent: struct {
        undefined_meta: Meta, // where undefined was set
        branch_meta: Meta, // where the conditional branch occurred
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
            .defined => unreachable,
            .inconsistent => unreachable, // use reportInconsistentBranches instead
        }
        return error.UseBeforeAssign;
    }

    pub fn reportInconsistentBranches(self: @This(), ctx: *Context) anyerror!void {
        try ctx.meta.print(ctx.writer, "use of value that may be undefined in ", .{});
        switch (self) {
            .inconsistent => |p| {
                try p.branch_meta.print(ctx.writer, "conditional branch has conflicting status at ", .{});
                if (p.var_name) |name| {
                    try p.undefined_meta.print(ctx.writer, "variable '{s}' was set to undefined in ", .{name});
                } else {
                    try p.undefined_meta.print(ctx.writer, "value set to undefined in ", .{});
                }
            },
            .defined => unreachable,
            .undefined => unreachable, // use reportUseBeforeAssign instead
        }
        return error.InconsistentBranches;
    }

    pub fn alloc(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Alloc) !void {
        _ = params;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = ctx.meta } });
    }

    pub fn alloc_create(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocCreate) !void {
        _ = ctx;
        _ = params;
        // The pointer itself is defined (it exists), pointee state is set by store
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
    }

    pub fn struct_field_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StructFieldPtr) !void {
        _ = ctx;
        _ = params;
        // The pointer itself is defined (it exists and points to a valid field)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
    }

    /// Helper to recursively set all scalars/pointers in a refinement tree to defined.
    fn setDefinedRecursive(refinements: *Refinements, idx: EIdx) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.undefined = .{ .defined = {} },
            .pointer => |*p| {
                p.analyte.undefined = .{ .defined = {} };
                setDefinedRecursive(refinements, p.to);
            },
            .optional => |o| setDefinedRecursive(refinements, o.to),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    setDefinedRecursive(refinements, field_idx);
                }
            },
            .future, .void, .unimplemented, .noreturn, .retval_future => {},
            .region => @panic("setDefinedRecursive: region not yet implemented"),
            .@"union" => @panic("setDefinedRecursive: union not yet implemented"),
        }
    }

    // Handlers for operations that produce defined scalar results
    pub fn cmp_eq(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Simple(.cmp_eq)) !void {
        _ = ctx;
        _ = params;
        const idx = results[index].refinement.?;
        refinements.at(idx).scalar.undefined = .{ .defined = {} };
    }

    pub fn add_with_overflow(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Unimplemented(.{})) !void {
        _ = results;
        _ = index;
        _ = ctx;
        _ = refinements;
        _ = params;
        // add_with_overflow is Unimplemented - it creates .unimplemented refinement, no undefined state needed
    }

    pub fn optional_payload(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.OptionalPayload) !void {
        _ = ctx;
        _ = params;
        const idx = results[index].refinement.?;
        refinements.at(idx).scalar.undefined = .{ .defined = {} };
    }

    /// br sets refinement on its target block, not on itself.
    /// When it creates scalars for the block, we need to set their undefined state.
    pub fn br(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Br) !void {
        _ = index;
        _ = ctx;
        // br sets refinement on self.block, not on its own index
        // When source is an eidx with existing refinement, undefined state is already set.
        // When br creates a new scalar (interned source), we need to set undefined.
        switch (params.src) {
            .eidx => {}, // Source has existing refinement with undefined state
            .interned => {
                const block_idx = results[params.block].refinement orelse return;
                setDefinedRecursive(refinements, block_idx);
            },
            .other => @panic("br: unexpected .other source"),
        }
    }

    /// Recursively set undefined state on a refinement and its children.
    /// Only called when storing an undefined value (is_undef = true).
    fn setUndefinedRecursive(refinements: *Refinements, idx: EIdx, undef_state: Undefined) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.undefined = undef_state,
            .pointer => |*p| {
                p.analyte.undefined = undef_state;
                setUndefinedRecursive(refinements, p.to, undef_state);
            },
            .optional => |o| {
                // Don't set analyte.undefined on optional - only the payload carries undefined state
                setUndefinedRecursive(refinements, o.to, undef_state);
            },
            .@"struct" => |s| {
                // Don't set analyte.undefined on struct - only the fields carry undefined state
                for (s.fields) |field_idx| {
                    setUndefinedRecursive(refinements, field_idx, undef_state);
                }
            },
            .future, .void, .unimplemented, .noreturn, .retval_future => {},
            .region => @panic("setUndefinedRecursive: region not yet implemented"),
            .@"union" => @panic("setUndefinedRecursive: union not yet implemented"),
        }
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

        if (params.is_undef) {
            // Undefined stores: mark pointee and all children as undefined (recursive)
            const pending_var_name = ctx.pending_var_name;
            const undef_state: Undefined = .{ .undefined = .{ .meta = ctx.meta, .var_name = pending_var_name } };
            setUndefinedRecursive(refinements, pointee_idx, undef_state);
        } else {
            // Defined stores: mark just the immediate pointee as defined (non-recursive)
            // For containers (optional/struct), we don't set their analyte - only scalars/pointers
            switch (refinements.at(pointee_idx).*) {
                .scalar => |*s| s.undefined = .{ .defined = {} },
                .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                // Containers don't track undefined on themselves, only on children
                .optional, .@"struct" => {},
                .future, .void, .unimplemented, .noreturn, .retval_future => {},
                .region => @panic("store: region not yet implemented"),
                .@"union" => @panic("store: union not yet implemented"),
            }
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
                // undefined is null when value wasn't allocated through our tracked mechanisms
                // (e.g., external data, FFI). No undefined tracking available - skip.
                const undef = s.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.undefined = .{ .defined = {} };
                    },
                }
            },
            .pointer => |ind| {
                // undefined is null when pointer wasn't allocated through our tracked mechanisms.
                // No undefined tracking available - skip.
                const undef = ind.analyte.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // For compound types, we share the entity so state is already correct.
                        // Only propagate for fresh scalars.
                        const idx = results[index].refinement orelse return;
                        switch (refinements.at(idx).*) {
                            .scalar => |*s| s.undefined = .{ .defined = {} },
                            else => {}, // Shared entity - state is already correct
                        }
                    },
                }
            },
            .optional => |o| {
                // Optional containers don't track undefined - check the payload
                // Recurse to check the payload's undefined state
                switch (refinements.at(o.to).*) {
                    .scalar => |s| {
                        const undef = s.undefined orelse return;
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    },
                    else => {}, // Other payload types - skip for now
                }
            },
            .@"struct" => {
                // Loading a struct doesn't error - individual field access will check
                // The loaded struct value will carry the field refinements
            },
            .region, .@"union" => @panic("load: pointee is compound type - undefined tracking not yet implemented"),
            .future => @panic("load: pointee is .future - was never stored to"),
            .unimplemented => @panic("load: pointee refinement is unimplemented"),
            else => |t| std.debug.panic("load: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        // ptr is null for debug info pointing to interned/global - no local tracking needed
        const inst = params.ptr orelse return;
        // refinement is null for uninitialized instructions - nothing to name yet
        const ptr_idx = results[inst].refinement orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
            .pointer => |ind| ind.to,
            .scalar => ptr_idx,
            // unimplemented/retval_future/void have no undefined tracking - skip naming
            .unimplemented, .retval_future, .void => return,
            else => @panic("unexpected refinement type in dbg_var_ptr (outer)"),
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |*s| {
                // undefined is null when value wasn't tracked - nothing to name
                const undef = &(s.undefined orelse return);
                switch (undef.*) {
                    .undefined => |*meta| meta.var_name = params.name,
                    .inconsistent => |*meta| meta.var_name = params.name,
                    .defined => {},
                }
            },
            .pointer => |*ind| {
                // undefined is null when pointer wasn't tracked - nothing to name
                const undef = &(ind.analyte.undefined orelse return);
                switch (undef.*) {
                    .undefined => |*meta| meta.var_name = params.name,
                    .inconsistent => |*meta| meta.var_name = params.name,
                    .defined => {},
                }
            },
            // Optional and struct containers don't track undefined on themselves
            .optional, .@"struct" => {},
            .future => |*f| f.name = params.name, // Store name for when store transforms the future
            // unimplemented values have no undefined tracking - skip naming
            .unimplemented => {},
            else => @panic("unexpected refinement type in dbg_var_ptr (pointee)"),
        }
    }

    pub fn struct_field_val(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StructFieldVal) !void {
        const result_idx = results[index].refinement.?;

        const operand = params.operand orelse {
            // Interned/global struct - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
            return;
        };
        const struct_ref = results[operand].refinement orelse {
            // No struct refinement - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
            return;
        };

        const s = switch (refinements.at(struct_ref).*) {
            .@"struct" => |st| st,
            else => {
                // Not a struct - result was created as fresh scalar, set to defined
                refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
                return;
            },
        };

        // Zig's bounds checking handles out-of-bounds access
        const field_idx = s.fields[params.field_index];

        // When field is in bounds, result shares field's refinement (undefined state already set).
        // Check if this field is undefined and report error if accessed.
        switch (refinements.at(field_idx).*) {
            .scalar => |sc| {
                const undef = sc.undefined orelse @panic("struct_field_val: field scalar has no undefined state");
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            .pointer => |ind| {
                const undef = ind.analyte.undefined orelse @panic("struct_field_val: field pointer has no undefined state");
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            // Optional and struct containers don't track undefined on themselves
            // Their children carry the undefined state
            .optional, .@"struct" => {},
            .future => {}, // Field is a future - not yet resolved
            else => |t| std.debug.panic("struct_field_val: unexpected field refinement type {s}", .{@tagName(t)}),
        }
    }

    // Backward propagation is handled centrally by Inst.backPropagate()

    /// Merge undefined states from two branches after a conditional.
    /// Walks the refinement tree and reports if branches have inconsistent initialization.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) !void {
        _ = merge_tag; // Available for context-specific merge behavior
        mergeRefinement(ctx, orig, true_branch, false_branch);
    }

    fn mergeRefinement(
        ctx: *Context,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) void {
        const orig_ref = orig[0].at(orig[1]);
        const true_ref = true_branch[0].at(true_branch[1]);
        const false_ref = false_branch[0].at(false_branch[1]);

        // Handle tombstoned futures - these represent early returns and are OK to merge
        const true_is_tombstoned = true_ref.* == .future and true_ref.future.tombstoned;
        const false_is_tombstoned = false_ref.* == .future and false_ref.future.tombstoned;

        if (true_is_tombstoned and false_is_tombstoned) {
            // Both branches returned early - nothing to merge
            return;
        }
        if (true_is_tombstoned) {
            // True branch returned early - use false branch's value
            orig_ref.* = false_ref.*;
            return;
        }
        if (false_is_tombstoned) {
            // False branch returned early - use true branch's value
            orig_ref.* = true_ref.*;
            return;
        }

        switch (orig_ref.*) {
            .scalar => |*s| {
                const true_undef = true_ref.scalar.undefined orelse return;
                const false_undef = false_ref.scalar.undefined orelse return;
                s.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
            },
            .pointer => |*p| {
                // Merge analyte on the pointer itself
                if (true_ref.pointer.analyte.undefined) |true_undef| {
                    if (false_ref.pointer.analyte.undefined) |false_undef| {
                        p.analyte.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
                    }
                }
                // Recursively merge pointee
                mergeRefinement(
                    ctx,
                    .{ orig[0], p.to },
                    .{ true_branch[0], true_ref.pointer.to },
                    .{ false_branch[0], false_ref.pointer.to },
                );
            },
            .optional => |*o| {
                // Optional containers don't track undefined - just recurse into payload
                mergeRefinement(
                    ctx,
                    .{ orig[0], o.to },
                    .{ true_branch[0], true_ref.optional.to },
                    .{ false_branch[0], false_ref.optional.to },
                );
            },
            .region => |*r| {
                if (true_ref.region.analyte.undefined) |true_undef| {
                    if (false_ref.region.analyte.undefined) |false_undef| {
                        r.analyte.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
                    }
                }
                mergeRefinement(
                    ctx,
                    .{ orig[0], r.to },
                    .{ true_branch[0], true_ref.region.to },
                    .{ false_branch[0], false_ref.region.to },
                );
            },
            // Types without undefined tracking - no merge needed
            .void, .unimplemented, .noreturn => {},
            .@"struct" => |*s| {
                // Struct containers don't track undefined - just recurse into fields
                for (s.fields, 0..) |field_idx, i| {
                    mergeRefinement(
                        ctx,
                        .{ orig[0], field_idx },
                        .{ true_branch[0], true_ref.@"struct".fields[i] },
                        .{ false_branch[0], false_ref.@"struct".fields[i] },
                    );
                }
            },
            // Types with undefined tracking not yet implemented
            .@"union" => @panic("mergeRefinement: union merge not yet implemented"),
            // Non-tombstoned futures should never appear in merges - they must be resolved first
            .future => @panic("mergeRefinement: non-tombstoned .future should have been resolved before merge"),
            .retval_future => @panic("mergeRefinement: cannot merge .retval_future"),
        }
    }

    fn mergeUndefinedStates(ctx: *Context, true_undef: Undefined, false_undef: Undefined) Undefined {
        // Handle .inconsistent propagation first - once inconsistent, stays inconsistent
        if (true_undef == .inconsistent) return true_undef;
        if (false_undef == .inconsistent) return false_undef;

        const true_is_defined = true_undef == .defined;
        const false_is_defined = false_undef == .defined;

        // Both agree
        if (true_is_defined and false_is_defined) return .{ .defined = {} };
        if (!true_is_defined and !false_is_defined) return true_undef; // both undefined, use first's meta

        // Inconsistent: one defined, one undefined -> return .inconsistent
        const undef_state = if (!true_is_defined) true_undef.undefined else false_undef.undefined;
        return .{ .inconsistent = .{
            .undefined_meta = undef_state.meta,
            .branch_meta = ctx.meta,
            .var_name = undef_state.var_name,
        } };
    }
};

const debug = @import("builtin").mode == .Debug;

/// Validate that refinements conform to undefined tracking rules:
/// - For .optional and .struct, the top-level analyte.undefined must be null
/// - For .scalar and .pointer, the analyte.undefined must be set (not null)
pub fn testValid(refinements: *Refinements) void {
    if (!debug) return;
    for (refinements.list.items) |ref| {
        switch (ref) {
            .scalar => |s| {
                if (s.undefined == null) {
                    @panic("ScalarMissingUndefinedState");
                }
            },
            .pointer => |p| {
                if (p.analyte.undefined == null) {
                    @panic("PointerMissingUndefinedState");
                }
            },
            .optional => |o| {
                if (o.analyte.undefined != null) {
                    @panic("OptionalContainerHasUndefinedState");
                }
            },
            .@"struct" => |s| {
                if (s.analyte.undefined != null) {
                    @panic("StructContainerHasUndefinedState");
                }
            },
            else => {},
        }
    }
}

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
    return ctx;
}

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_eidx = 0,
        .caller_refinements = null,
    };
}

test "alloc creates pointer to typed pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer to typed pointee)
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // alloc creates pointer; pointee type is determined by .ty parameter
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(pointee_idx).*));
    testValid(&refinements);
}

test "alloc_create creates pointer to future" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Use Inst.apply which calls tag.AllocCreate.apply (creates pointer to future)
    try Inst.apply(state, 1, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } });

    // alloc_create creates pointer; pointee is .future (structure determined by first store)
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.future, std.meta.activeTag(refinements.at(pointee_idx).*));
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
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store with is_undef=true
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } }, .is_undef = true } });

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
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store with is_undef=false
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } }, .is_undef = false } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "store with .null to optional sets inner to defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc at instruction 1 with optional type, then store null
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .optional = &.{ .scalar = {} } } } });
    // Store null to the optional - inner should be defined (null is a valid defined value)
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .null = &.{ .scalar = {} } } }, .is_undef = false } });

    // Check the pointee is an optional
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.optional, std.meta.activeTag(refinements.at(pointee_idx).*));

    // Check the optional's inner value is a defined scalar
    const inner_idx = refinements.at(pointee_idx).optional.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(inner_idx).*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(inner_idx).scalar.undefined.?));
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
        Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } }),
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

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } });
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

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } });
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
