const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
const Context = @import("../Context.zig");
const tag = @import("../tag.zig");
const Meta = @import("../Meta.zig");
const State = @import("../lib.zig").State;

/// Validate that variant_safety state is consistent with the refinement.
/// Called by Refinements.testValid for each refinement.
pub fn testValid(refinement: Refinements.Refinement) void {
    switch (refinement) {
        .@"union" => |u| {
            if (u.analyte.variant_safety) |vs| {
                vs.testValidForUnion(u);
            }
        },
        // variant_safety should only exist on unions - check that other types don't have it
        .scalar => |s| {
            if (s.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got scalar", .{});
            }
        },
        inline .pointer, .optional, .errorunion, .@"struct" => |data, t| {
            if (data.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got {s}", .{@tagName(t)});
            }
        },
        // No analyte on void, noreturn, etc.
        .void, .noreturn, .retval_future, .unimplemented, .region => {},
    }
}

/// VariantSafety tracks which union variants are active and where they were set.
/// The active_metas slice has one entry per union field:
/// - null means the field is inactive
/// - non-null Meta means the field is active (with location where it was set)
pub const VariantSafety = struct {
    active_metas: []?Meta,

    /// Validate that variant_safety state is consistent with the union refinement.
    /// - If active_metas[i] is non-null, fields[i] must also be non-null
    /// - active_metas[i] being null is always valid (either field is inactive or ambiguous after merge)
    fn testValidForUnion(self: VariantSafety, u: Refinements.Refinement.Union) void {
        if (self.active_metas.len != u.fields.len) {
            std.debug.panic(
                "VariantSafety.testValid: active_metas.len ({d}) != fields.len ({d})",
                .{ self.active_metas.len, u.fields.len },
            );
        }
        for (self.active_metas, u.fields, 0..) |meta_opt, field_opt, i| {
            // If meta says field is active, the field entity must exist
            // (But null meta is always valid - could be inactive or ambiguous after merge)
            if (meta_opt != null and field_opt == null) {
                std.debug.panic(
                    "VariantSafety.testValid: meta says field {d} is active but no field entity exists",
                    .{i},
                );
            }
        }
    }

    /// Handle set_union_tag - update active_metas to reflect new active variant
    pub fn set_union_tag(state: State, index: usize, params: tag.SetUnionTag) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Follow pointer to get to union - Zig's safety checks will panic on wrong types
        const ptr_ref = results[params.ptr.?].refinement.?;
        const container_idx = refinements.at(ptr_ref).pointer.to;
        const u = &refinements.at(container_idx).@"union";
        const field_idx = params.field_index.?;

        // Get or create variant_safety
        if (u.analyte.variant_safety) |*vs| {
            // Update existing: set all to null, then set active field
            for (vs.active_metas) |*m| m.* = null;
            vs.active_metas[field_idx] = ctx.meta;
        } else {
            // Create new active_metas array
            const active_metas = ctx.allocator.alloc(?Meta, u.fields.len) catch @panic("out of memory");
            for (active_metas) |*m| m.* = null;
            active_metas[field_idx] = ctx.meta;
            u.analyte.variant_safety = .{ .active_metas = active_metas };
        }
    }

    /// Check struct_field_ptr access on unions - report error if accessing inactive variant
    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // base can be null for interned/global pointers - skip variant checking for those
        const base = params.base orelse return;
        const base_ref = results[base].refinement orelse return;

        // Follow pointer to container - must be a pointer
        const container_idx = refinements.at(base_ref).pointer.to;

        // Only check variant access for unions
        if (refinements.at(container_idx).* == .@"union") {
            try checkVariantAccess(refinements.at(container_idx).@"union", params.field_index, ctx);
        }
    }

    /// Handle cond_br - tag checks validate but don't change the active variant.
    /// Only set_union_tag/union_init should modify variant_safety.
    /// This is intentionally a no-op: the original variant set by set_union_tag
    /// determines what's safe to access, not tag checks.
    pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
        _ = state;
        _ = index;
        _ = params;
        // No-op: tag checks don't change which variant is active.
        // The original set_union_tag establishes the active variant.
    }

    /// Check struct_field_val access on unions - report error if accessing inactive variant
    pub fn struct_field_val(state: State, index: usize, params: tag.StructFieldVal) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // operand can be null for interned values - skip variant checking for those
        const operand = params.operand orelse return;
        const container_ref = results[operand].refinement orelse return;

        // Only check variant access for unions
        if (refinements.at(container_ref).* == .@"union") {
            try checkVariantAccess(refinements.at(container_ref).@"union", params.field_index, ctx);
        }
    }

    fn checkVariantAccess(u: Refinements.Refinement.Union, field_index: usize, ctx: *Context) !void {
        const vs = u.analyte.variant_safety orelse return;

        // Count active variants and collect their info
        var active_count: usize = 0;
        var first_active_idx: ?usize = null;
        var second_active_idx: ?usize = null;
        for (vs.active_metas, 0..) |meta_opt, i| {
            if (meta_opt != null) {
                active_count += 1;
                if (first_active_idx == null) {
                    first_active_idx = i;
                } else if (second_active_idx == null) {
                    second_active_idx = i;
                }
            }
        }

        // If multiple variants are active, state is ambiguous - error on any access
        if (active_count > 1) {
            const first_idx = first_active_idx.?;
            const second_idx = second_active_idx.?;

            // Get field names
            const first_name: ?[]const u8 = if (u.field_names.len > first_idx) u.field_names[first_idx] else null;
            const second_name: ?[]const u8 = if (u.field_names.len > second_idx) u.field_names[second_idx] else null;

            try ctx.meta.print(ctx.writer, "access of union with ambiguous active variant in ", .{});

            // Report where both variants were set
            if (first_name) |name| {
                try vs.active_metas[first_idx].?.print(ctx.writer, "variant '{s}' set in ", .{name});
            } else {
                try vs.active_metas[first_idx].?.print(ctx.writer, "variant set in ", .{});
            }
            if (second_name) |name| {
                try vs.active_metas[second_idx].?.print(ctx.writer, "variant '{s}' set in ", .{name});
            } else {
                try vs.active_metas[second_idx].?.print(ctx.writer, "variant set in ", .{});
            }

            return error.AmbiguousVariantAccess;
        }

        // Check if this field is inactive (and exactly one other is active)
        if (vs.active_metas[field_index] == null) {
            // Find which field IS active for the error message
            var active_meta: ?Meta = null;
            var active_name: ?[]const u8 = null;
            for (vs.active_metas, 0..) |meta_opt, i| {
                if (meta_opt) |meta| {
                    active_meta = meta;
                    if (u.field_names.len > i) {
                        active_name = u.field_names[i];
                    }
                    break;
                }
            }

            // Get accessed field name
            const accessed_name: ?[]const u8 = if (u.field_names.len > field_index)
                u.field_names[field_index]
            else
                null;

            // Report the access error
            if (accessed_name) |name| {
                try ctx.meta.print(ctx.writer, "access of inactive union variant '{s}' in ", .{name});
            } else {
                try ctx.meta.print(ctx.writer, "access of inactive union variant in ", .{});
            }

            // Report where the active variant was set
            if (active_meta) |meta| {
                if (active_name) |name| {
                    try meta.print(ctx.writer, "active variant '{s}' set in ", .{name});
                } else {
                    try meta.print(ctx.writer, "active variant set in ", .{});
                }
            }

            return error.InactiveVariantAccess;
        }
    }

    /// Merge variant_safety state from two branches.
    /// If both branches have the same active variant, keep it.
    /// If branches have different active variants, mark both as active (ambiguous).
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
            .@"union" => |*u| {
                // Get variant_safety from both branches (may be null)
                const true_vs: ?VariantSafety = if (true_ref.* == .@"union")
                    true_ref.@"union".analyte.variant_safety
                else
                    null;
                const false_vs: ?VariantSafety = if (false_ref.* == .@"union")
                    false_ref.@"union".analyte.variant_safety
                else
                    null;

                // If neither branch has variant_safety, nothing to merge
                if (true_vs == null and false_vs == null) return;

                // If only one branch has variant_safety, use that
                if (true_vs == null) {
                    u.analyte.variant_safety = false_vs;
                    return;
                }
                if (false_vs == null) {
                    u.analyte.variant_safety = true_vs;
                    return;
                }

                // Both branches have variant_safety - check if they agree
                const tvs = true_vs.?;
                const fvs = false_vs.?;

                // Get or create variant_safety on orig
                if (u.analyte.variant_safety == null) {
                    // Create a new active_metas array
                    const allocator = orig[0].list.allocator;
                    const active_metas = allocator.alloc(?Meta, tvs.active_metas.len) catch @panic("out of memory");
                    for (active_metas) |*m| m.* = null;
                    u.analyte.variant_safety = .{ .active_metas = active_metas };
                }
                const orig_vs = &u.analyte.variant_safety.?;

                // Merge: mark as active any variant that was active in EITHER branch
                // If both branches have the same variant active, we get one active variant
                // If branches have different variants active, we get multiple active (ambiguous)
                // Also ensure field entities exist for any active variants
                for (orig_vs.active_metas, 0..) |*orig_m, i| {
                    const true_m = tvs.active_metas[i];
                    const false_m = fvs.active_metas[i];
                    if (true_m != null) {
                        orig_m.* = true_m;
                        // Ensure field entity exists - create new in orig refinements
                        if (u.fields[i] == null) {
                            u.fields[i] = orig[0].appendEntity(.{ .scalar = .{ .undefined = .{ .defined = {} } } }) catch @panic("out of memory");
                        }
                    } else if (false_m != null) {
                        orig_m.* = false_m;
                        // Ensure field entity exists - create new in orig refinements
                        if (u.fields[i] == null) {
                            u.fields[i] = orig[0].appendEntity(.{ .scalar = .{ .undefined = .{ .defined = {} } } }) catch @panic("out of memory");
                        }
                    } else {
                        orig_m.* = null;
                    }
                }
            },
            .pointer => |p| {
                // Follow pointer to pointee
                if (true_ref.* != .pointer or false_ref.* != .pointer) return;
                mergeRefinement(
                    .{ orig[0], p.to },
                    .{ true_branch[0], true_ref.pointer.to },
                    .{ false_branch[0], false_ref.pointer.to },
                );
            },
            .optional => |o| {
                if (true_ref.* != .optional or false_ref.* != .optional) return;
                mergeRefinement(
                    .{ orig[0], o.to },
                    .{ true_branch[0], true_ref.optional.to },
                    .{ false_branch[0], false_ref.optional.to },
                );
            },
            .errorunion => |e| {
                if (true_ref.* != .errorunion or false_ref.* != .errorunion) return;
                mergeRefinement(
                    .{ orig[0], e.to },
                    .{ true_branch[0], true_ref.errorunion.to },
                    .{ false_branch[0], false_ref.errorunion.to },
                );
            },
            else => {},
        }
    }
};
