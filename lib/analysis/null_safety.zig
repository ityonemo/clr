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
    // the result of null and non_null merged, or 
    // the result of a newly created nullable that hasn't been initialized.
    unknown: void,
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

    /// is_non_null is a pure operation - it tests nullity and returns a boolean.
    /// It does NOT modify the optional's analyte. The narrowing happens in cond_br.
    pub fn is_non_null(state: State, index: usize, params: tag.IsNonNull) !void {
        _ = state;
        _ = index;
        _ = params;
        // Pure operation - does nothing to the analyte
    }

    /// is_null is a pure operation - it tests nullity and returns a boolean.
    /// It does NOT modify the optional's analyte. The narrowing happens in cond_br.
    pub fn is_null(state: State, index: usize, params: tag.IsNull) !void {
        _ = state;
        _ = index;
        _ = params;
        // Pure operation - does nothing to the analyte
    }

    /// wrap_optional constructs a known non-null optional.
    pub fn wrap_optional(state: State, index: usize, params: tag.WrapOptional) !void {
        _ = params;
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);
        if (result_ref.* != .optional) return;
        result_ref.optional.analyte.null_safety = .{ .non_null = state.ctx.meta };
    }

    /// bitcast from a pointer to an optional-pointer preserves a definitely non-null value.
    pub fn bitcast(state: State, index: usize, params: tag.Bitcast) !void {
        if (params.ty != .optional or params.ty.optional.* != .pointer) return;

        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);
        if (result_ref.* != .optional) return;

        const src_gid: ?Gid = switch (params.src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => |interned| state.refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        const src = src_gid orelse return;
        if (state.refinements.at(src).* != .pointer) return;

        result_ref.optional.analyte.null_safety = .{ .non_null = state.ctx.meta };
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

        // Only narrow if state is unchecked (null) or .unknown - don't overwrite known state
        const ns = opt_ref.optional.analyte.null_safety;
        if (ns != null and ns.? != .unknown) return;

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

        const ns = ref.optional.analyte.null_safety orelse {
            // No null_safety set - check for preceding is_non_null assertion
            // Zig generates is_non_null before optional_payload for .? operator
            if (hasIsNonNullAssertion(results, index, params.src)) return;
            return reportUncheckedUnwrap(ctx, results[src_idx].name_id);
        };
        switch (ns) {
            .unknown => {
                // Unknown state means we tracked this optional and know it might be null
                // (e.g., after branch merge). This is an error regardless of runtime checks.
                return reportUncheckedUnwrap(ctx, results[src_idx].name_id);
            },
            .@"null" => return ns.reportNullUnwrap(ctx, results[src_idx].name_id),
            .non_null => {}, // Safe
        }
    }

    /// Check if there's a preceding is_non_null assertion on the same source.
    /// Zig generates is_non_null as a runtime safety check before .? (optional_payload).
    ///
    /// This is ONLY used when null_safety is NOT set (we have no tracking for this optional).
    /// In that case, the runtime check prevents undefined behavior, so we allow it.
    ///
    /// This is NOT used when null_safety is .unknown - that means we tracked the optional
    /// and know it might be null (e.g., after branch merge). That's a static analysis error.
    fn hasIsNonNullAssertion(results: []const Inst, index: usize, target_src: tag.Src) bool {
        // Look backward for is_non_null on the same source
        var i: usize = index;
        while (i > 0) {
            i -= 1;
            const inst_tag = results[i].inst_tag orelse continue;
            switch (inst_tag) {
                .is_non_null => |t| {
                    if (std.meta.eql(t.src, target_src)) {
                        return true; // Found matching is_non_null assertion
                    }
                },
                else => {},
            }
        }
        return false;
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

        // Only optionals can be marked as null
        const ref = refinements.at(gid);
        if (ref.* != .optional) return;

        // Mark based on is_null_opt
        if (is_null_opt) {
            ref.optional.analyte.null_safety = .{ .@"null" = .{
                .function = "",
                .file = loc.file,
                .line = loc.line,
                .column = loc.column,
            } };
        } else {
            ref.optional.analyte.null_safety = .{ .non_null = .{
                .function = "",
                .file = loc.file,
                .line = loc.line,
                .column = loc.column,
            } };
        }
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

/// Validate that a refinement conforms to null_safety rules:
/// - MUST EXIST: .optional
/// - MUST BE NULL: .scalar, .pointer, .errorunion, .struct, .union, .recursive, .fnptr, .allocator, .region
/// - NO ANALYTE: .void, .noreturn, .unimplemented
pub fn testValid(refinement: Refinements.Refinement) void {
    switch (refinement) {
        // null_safety must exist on optionals
        .optional => |o| {
            if (o.analyte.null_safety == null) {
                std.debug.panic("null_safety must be set on optionals", .{});
            }
        },
        // null_safety must be null on non-optional types
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
        inline .pointer, .errorunion, .@"struct", .@"union", .recursive, .fnptr, .region => |data, t| {
            if (data.analyte.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals, got {s}", .{@tagName(t)});
            }
        },
        .void, .noreturn, .unimplemented => {},
    }
}
