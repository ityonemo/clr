const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
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

    /// is_non_null sets the optional's null_safety to .unknown with check info
    pub fn is_non_null(state: State, index: usize, params: tag.IsNonNull) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const src_idx = switch (params.src) {
            .eidx => |s| results[s].refinement orelse return,
            else => return, // Comptime - no tracking needed
        };

        // is_non_null can apply to optionals or pointers
        const ref = refinements.at(src_idx);
        const analyte = switch (ref.*) {
            .optional => |*o| &o.analyte,
            .pointer => |*p| &p.analyte,
            else => return, // Other types (e.g., error unions) - no null tracking
        };

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

    /// is_null sets the optional's null_safety to .unknown with check info
    pub fn is_null(state: State, index: usize, params: tag.IsNull) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const src_idx = switch (params.src) {
            .eidx => |s| results[s].refinement orelse return,
            else => return, // Comptime - no tracking needed
        };

        // is_null can apply to optionals or pointers
        const ref = refinements.at(src_idx);
        const analyte = switch (ref.*) {
            .optional => |*o| &o.analyte,
            .pointer => |*p| &p.analyte,
            else => return, // Other types (e.g., error unions) - no null tracking
        };

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

    fn reportUncheckedUnwrap(ctx: *Context, name: ?[]const u8) anyerror!void {
        if (name) |n| {
            try ctx.meta.print(ctx.writer, "unchecked optional unwrap of '{s}' in ", .{n});
        } else {
            try ctx.meta.print(ctx.writer, "unchecked optional unwrap in ", .{});
        }
        return error.UncheckedOptionalUnwrap;
    }

    fn reportNullUnwrap(self: @This(), ctx: *Context, name: ?[]const u8) anyerror!void {
        if (name) |n| {
            try ctx.meta.print(ctx.writer, "optional unwrap of known null '{s}' in ", .{n});
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
            .eidx => |s| s,
            else => return, // Comptime - always safe
        };

        const optional_idx = results[src_idx].refinement orelse return;
        const ref = refinements.at(optional_idx);
        // optional_payload can apply to optionals or pointers (for error union unwrapping)
        if (ref.* != .optional) return;

        const ns = ref.optional.analyte.null_safety orelse return reportUncheckedUnwrap(ctx, results[src_idx].name);
        switch (ns) {
            .unknown => return reportUncheckedUnwrap(ctx, results[src_idx].name),
            .@"null" => return ns.reportNullUnwrap(ctx, results[src_idx].name),
            .non_null => {}, // Safe
        }
    }

    /// store tracks assignments to optionals
    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const ptr = params.ptr orelse return;
        const ptr_idx = results[ptr].refinement orelse return;
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        const pointee_idx = ptr_ref.pointer.to;
        const pointee = refinements.at(pointee_idx);
        if (pointee.* != .optional) return;

        // Check if we're storing null or a value
        switch (params.src) {
            .interned => |ty| {
                if (ty == .@"null") {
                    pointee.optional.analyte.null_safety = .{ .@"null" = ctx.meta };
                } else {
                    pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
                }
            },
            .eidx => {
                // Runtime value - mark as non_null
                pointee.optional.analyte.null_safety = .{ .non_null = ctx.meta };
            },
            .other => @panic("store: unexpected .other src for optional"),
        }
    }

    /// Merge null_safety states after conditional branches
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) !void {
        _ = ctx;
        _ = merge_tag;
        mergeRefinement(orig, true_branch, false_branch);
    }

    fn mergeRefinement(
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) void {
        const orig_ref = orig[0].at(orig[1]);
        const true_ref = true_branch[0].at(true_branch[1]);
        const false_ref = false_branch[0].at(false_branch[1]);

        switch (orig_ref.*) {
            .optional => |*o| {
                const true_ns = true_ref.optional.analyte.null_safety orelse return;
                const false_ns = false_ref.optional.analyte.null_safety orelse return;
                o.analyte.null_safety = mergeNullStates(true_ns, false_ns);
            },
            .pointer => |p| {
                // Follow pointer to pointee (which might be an optional)
                mergeRefinement(
                    .{ orig[0], p.to },
                    .{ true_branch[0], true_ref.pointer.to },
                    .{ false_branch[0], false_ref.pointer.to },
                );
            },
            else => {},
        }
    }

    fn mergeNullStates(true_ns: NullSafety, false_ns: NullSafety) NullSafety {
        // Same tag - keep it
        if (std.meta.activeTag(true_ns) == std.meta.activeTag(false_ns)) {
            return true_ns;
        }
        // Different states - reset to unknown (requires re-check)
        return .{ .unknown = null };
    }
};

pub fn testValid(refinement: Refinements.Refinement) void {
    switch (refinement) {
        // null_safety is valid on optionals and pointers (for ?*T pointer-like optionals)
        .optional, .pointer => {},
        // null_safety should not exist on non-optional/non-pointer types
        .scalar => |s| {
            if (s.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals/pointers, got scalar", .{});
            }
        },
        inline .errorunion, .@"struct", .@"union" => |data, t| {
            if (data.analyte.null_safety != null) {
                std.debug.panic("null_safety should only exist on optionals/pointers, got {s}", .{@tagName(t)});
            }
        },
        .void, .noreturn, .retval_future, .unimplemented, .region => {},
    }
}
