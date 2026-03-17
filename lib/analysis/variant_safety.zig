const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const Context = @import("../Context.zig");
const tag = @import("../tag.zig");
const core = @import("../core.zig");
const Meta = core.Meta;
const State = @import("../lib.zig").State;
const memory_safety = @import("memory_safety.zig");
const UndefinedSafety = @import("undefined_safety.zig").UndefinedSafety;

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
            if (s.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got scalar", .{});
            }
        },
        .allocator => |a| {
            if (a.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got allocator", .{});
            }
        },
        inline .pointer, .optional, .errorunion, .@"struct", .recursive, .fnptr => |data, t| {
            if (data.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got {s}", .{@tagName(t)});
            }
        },
        // No analyte on void, noreturn, etc.
        .void, .noreturn, .unimplemented, .region => {},
    }
}

/// VariantSafety tracks which union variants are active and where they were set.
/// The active_metas slice has one entry per union field:
/// - null means the field is inactive
/// - non-null Meta means the field is active (with location where it was set)
pub const VariantSafety = struct {
    active_metas: []?Meta,

    /// Deep copy - allocates new slice for active_metas.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        const new_active_metas = try allocator.alloc(?Meta, self.active_metas.len);
        @memcpy(new_active_metas, self.active_metas);
        return .{ .active_metas = new_active_metas };
    }

    /// Free allocated resources.
    pub fn deinit(self: @This(), allocator: std.mem.Allocator) void {
        allocator.free(self.active_metas);
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        for (self.active_metas) |am| {
            hasher.update(&.{@as(u8, if (am != null) 1 else 0)});
        }
    }

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
        for (self.active_metas, u.fields, 0..) |active_meta, field_opt, i| {
            // If field is marked active, the field entity must exist
            // (null active_meta is always valid - could be inactive or ambiguous after merge)
            if (active_meta != null and field_opt == null) {
                std.debug.panic(
                    "VariantSafety.testValid: field {d} marked active but no field entity exists",
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

        // Get the union pointer's refinement based on source type
        const ptr_ref: Gid = switch (params.ptr) {
            .inst => |inst| results[inst].refinement.?,
            .interned => |interned| refinements.getGlobal(interned.ip_idx).?,
            .fnptr => return, // function pointer - no variant tracking
        };
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

        // Get base pointer refinement - handle instruction or global base
        const base_ref: Gid = switch (params.base) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // function pointer - no variant checking
        };

        // Follow pointer to container - must be a pointer
        const container_idx = refinements.at(base_ref).pointer.to;

        // Only check variant access for unions
        if (refinements.at(container_idx).* == .@"union") {
            try checkVariantAccess(refinements.at(container_idx).@"union", params.field_index, ctx);
        }
    }

    /// Handle cond_br - when a tag check validates, update variant_safety to reflect
    /// what we know in this branch.
    ///
    /// ## Local vs Global Union Handling
    ///
    /// **Local unions**: The compiler reuses the same loaded value for both the check
    /// and subsequent field access. We update the local copy's variant_safety.
    ///
    /// **Global unions**: The compiler generates SEPARATE load instructions for each
    /// access. We must ALSO update the global's variant_safety so subsequent loads
    /// (via semideepCopy) get the correct state within this branch.
    pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Only handle if there's union tag info and this is the true branch
        const union_check = params.union_tag orelse return;
        if (!params.branch) return; // Only update on true branch

        // Find the union's refinement via the operand instruction
        const union_eidx = results[union_check.union_inst].refinement orelse return;
        const union_ref = refinements.at(union_eidx);
        if (union_ref.* != .@"union") return;

        const field_index = union_check.field_index;

        // Only update variant_safety if no variant is currently known as active.
        // If set_union_tag was called earlier (in this or another function), we already
        // have variant state and the cond_br is just a runtime safety check.
        // Unlike switch_br (user's explicit control flow), cond_br is a compiler-generated
        // safety check that should NOT override known state.
        const u = &union_ref.@"union";
        const vs = &(u.analyte.variant_safety orelse return);

        // Check field index bounds
        if (field_index >= u.fields.len) return;

        var has_known_active = false;
        for (vs.active_metas) |m| {
            if (m != null) {
                has_known_active = true;
                break;
            }
        }

        if (!has_known_active) {
            // No known active variant - use the cond_br check to establish it.
            // Create field entity if it doesn't exist
            // NOTE: We must re-fetch the union after appendEntity since it may reallocate the list
            if (u.fields[field_index] == null) {
                // Save undefined_safety before appendEntity (which may invalidate u)
                const undef_safety: UndefinedSafety = u.analyte.undefined_safety orelse .{ .defined = {} };
                const new_gid = refinements.appendEntity(.{ .scalar = .{
                    .analyte = .{ .undefined_safety = undef_safety },
                } }) catch return;
                // Re-fetch after potential reallocation
                refinements.at(union_eidx).@"union".fields[field_index] = new_gid;
            }

            // Re-fetch union and update local copy's variant_safety
            const u_ref = &refinements.at(union_eidx).@"union";
            const vs_ref = &(u_ref.analyte.variant_safety orelse return);
            for (vs_ref.active_metas) |*meta| {
                meta.* = null;
            }
            vs_ref.active_metas[field_index] = ctx.meta;
        }

        // For globals: also update the global's variant_safety so subsequent loads see it
        const inst_tag = results[union_check.union_inst].inst_tag orelse return;
        if (inst_tag != .load) return;
        const load_src = inst_tag.load.ptr;
        if (load_src != .interned) return; // Not an interned value
        const global_ip_idx = load_src.interned.ip_idx;

        const global_ptr_gid = refinements.getGlobal(global_ip_idx) orelse return;
        const global_pointee_gid = refinements.at(global_ptr_gid).pointer.to;
        const global_union_ref = refinements.at(global_pointee_gid);
        if (global_union_ref.* != .@"union") return;

        const global_u = &global_union_ref.@"union";

        // Check field index bounds
        if (field_index >= global_u.fields.len) return;

        // For globals, we always update based on the cond_br check.
        // Unlike locals where set_union_tag in the same function gives us accurate info,
        // globals may have been modified by other functions. The cond_br check is our
        // only source of truth in this function's analysis context.
        // Create field entity if it doesn't exist
        // NOTE: We must re-fetch the union after appendEntity since it may reallocate the list
        if (global_u.fields[field_index] == null) {
            // Save undefined_safety before appendEntity (which may invalidate global_u)
            const undef_safety: UndefinedSafety = global_u.analyte.undefined_safety orelse .{ .defined = {} };
            const new_gid = refinements.appendEntity(.{ .scalar = .{
                .analyte = .{ .undefined_safety = undef_safety },
            } }) catch return;
            // Re-fetch after potential reallocation
            refinements.at(global_pointee_gid).@"union".fields[field_index] = new_gid;
        }

        // Re-fetch union and clear all active variants, set only this one
        const global_u_ref = &refinements.at(global_pointee_gid).@"union";
        const global_vs_ref = &(global_u_ref.analyte.variant_safety orelse return);
        for (global_vs_ref.active_metas) |*meta| {
            meta.* = null;
        }
        global_vs_ref.active_metas[field_index] = ctx.meta;
    }

    /// Handle switch_br - when switching on a union tag, update the active variant.
    /// In each switch case, we know which variant is active based on the case's items.
    ///
    /// We update BOTH the loaded value AND the source pointer's pointee (if any).
    /// This ensures subsequent loads from the same pointer get the updated variant state.
    pub fn switch_br(state: State, index: usize, params: tag.SwitchBr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // If this switch case has union_tag info, update the active variant
        const union_check = params.union_tag orelse return;

        // Find the union's refinement via the operand instruction
        const union_eidx = results[union_check.union_inst].refinement orelse return;
        const union_ref = refinements.at(union_eidx);

        // Only update for union refinements
        if (union_ref.* != .@"union") return;

        const field_index = union_check.field_index;

        // Update the loaded value's variant_safety
        try updateVariantForUnion(refinements, union_eidx, field_index, ctx);

        // Also update the source pointer's pointee, so subsequent loads get the updated state
        const inst_tag = results[union_check.union_inst].inst_tag orelse return;
        if (inst_tag != .load) return;
        const load_src = inst_tag.load.ptr;

        const ptr_gid: ?Gid = switch (load_src) {
            .inst => |inst| results[inst].refinement,
            .interned => |interned| refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        const ptr_ref = ptr_gid orelse return;
        const pointee_gid = refinements.at(ptr_ref).pointer.to;

        // Only update if pointee is a union (it should be)
        if (refinements.at(pointee_gid).* != .@"union") return;

        try updateVariantForUnion(refinements, pointee_gid, field_index, ctx);
    }

    /// Helper to update variant_safety for a union at the given gid.
    /// Creates field entity if needed, clears all active variants, sets the specified one.
    fn updateVariantForUnion(refinements: *Refinements, union_gid: Gid, field_index: usize, ctx: *Context) !void {
        const union_ref = refinements.at(union_gid);
        if (union_ref.* != .@"union") return;

        const u = &union_ref.@"union";
        const vs = &(u.analyte.variant_safety orelse return);

        // Check field index bounds
        if (field_index >= vs.active_metas.len) return;

        // Create field entity if it doesn't exist
        // NOTE: We must re-fetch the union after appendEntity since it may reallocate the list
        if (field_index < u.fields.len and u.fields[field_index] == null) {
            // Save undefined_safety before appendEntity (which may invalidate u)
            const undef_safety: UndefinedSafety = u.analyte.undefined_safety orelse .{ .defined = {} };
            const new_gid = refinements.appendEntity(.{ .scalar = .{
                .analyte = .{ .undefined_safety = undef_safety },
            } }) catch return;
            // Re-fetch after potential reallocation
            refinements.at(union_gid).@"union".fields[field_index] = new_gid;
        }

        // Re-fetch union and update variant_safety
        const u_ref = &refinements.at(union_gid).@"union";
        const vs_ref = &(u_ref.analyte.variant_safety orelse return);

        // Clear all active variants and set only this one
        for (vs_ref.active_metas) |*meta| {
            meta.* = null;
        }
        vs_ref.active_metas[field_index] = ctx.meta;
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

            // Get field names via ctx.getName
            // Note: Field names are stored in Type, not Refinement. For now, use null.
            // TODO: Pass union type to get field names for better error messages.
            const first_name: ?[]const u8 = null;
            const second_name: ?[]const u8 = null;

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
            for (vs.active_metas) |meta_opt| {
                if (meta_opt) |meta| {
                    active_meta = meta;
                    break;
                }
            }

            // Get field names - note: stored in Type, not Refinement. For now, use null.
            // TODO: Pass union type to get field names for better error messages.
            const active_name: ?[]const u8 = null;
            const accessed_name: ?[]const u8 = null;

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

    /// Merge variant_safety state from N branches for a single union node.
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

        // Only unions have variant_safety
        if (orig_ref.* != .@"union") return;
        const u = &orig_ref.@"union";

        // Gather variant_safety from all reachable branches
        var first_vs: ?VariantSafety = null;

        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;
            const branch_ref = branch.refinements.at(branch_gid);
            if (branch_ref.* != .@"union") continue;

            const branch_vs = branch_ref.@"union".analyte.variant_safety orelse continue;
            if (first_vs == null) {
                first_vs = branch_vs;
            }
        }

        // If no branch has variant_safety, nothing to merge
        const vs = first_vs orelse return;

        // Get or create variant_safety on orig
        if (u.analyte.variant_safety == null) {
            const allocator = refinements.list.allocator;
            const active_metas = allocator.alloc(?Meta, vs.active_metas.len) catch @panic("out of memory");
            for (active_metas) |*m| m.* = null;
            u.analyte.variant_safety = .{ .active_metas = active_metas };
        }

        // Merge: mark as active any variant that was active in ANY reachable branch
        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;
            const branch_ref = branch.refinements.at(branch_gid);
            if (branch_ref.* != .@"union") continue;

            const branch_vs = branch_ref.@"union".analyte.variant_safety orelse continue;
            // Re-fetch orig_vs each iteration since copyTo below may reallocate
            const current_orig_vs = &refinements.at(orig_gid).@"union".analyte.variant_safety.?;
            // Skip if different lengths (shouldn't happen for same union type)
            if (current_orig_vs.active_metas.len != branch_vs.active_metas.len) continue;
            for (current_orig_vs.active_metas, branch_vs.active_metas, 0..) |*orig_m, branch_m, i| {
                if (branch_m != null) {
                    orig_m.* = branch_m;
                    // Ensure field entity exists - copy from branch if original is null
                    // NOTE: We must re-fetch the union after copyTo since it may reallocate the list
                    const current_u = &refinements.at(orig_gid).@"union";
                    if (current_u.fields[i] == null) {
                        const branch_u = branch_ref.@"union";
                        if (branch_u.fields[i]) |branch_field_gid| {
                            // Cross-table copy: properly copies pointer .to fields
                            const branch_field_ref = branch.refinements.at(branch_field_gid).*;
                            const copied_gid = Refinements.Refinement.copyTo(branch_field_ref, branch.refinements, refinements) catch @panic("out of memory");
                            // Re-fetch after potential reallocation from copyTo
                            refinements.at(orig_gid).@"union".fields[i] = copied_gid;
                            // Ensure memory_safety is set on the copied entity
                            // (branch may have created it via typeToRefinement without memory_safety)
                            memory_safety.MemorySafety.initUnsetRecursive(refinements, copied_gid);
                        }
                    }
                }
            }
        }
    }

    /// Initialize variant_safety for a global union.
    /// Sets the active_metas based on which field entity exists (was initialized).
    pub fn init_global(
        refinements: *Refinements,
        ptr_gid: Gid,
        pointee_gid: Gid,
        ctx: *Context,
        is_undefined: bool,
        is_null_opt: bool,
        loc: tag.GlobalLocation,
        field_info: ?tag.GlobalFieldInfo,
    ) void {
        _ = ptr_gid;
        _ = ctx;
        _ = is_undefined;
        _ = is_null_opt;
        _ = field_info;

        // Only unions have variant_safety
        const pointee = refinements.at(pointee_gid);
        if (pointee.* != .@"union") return;

        const u = &pointee.@"union";
        const allocator = refinements.list.allocator;

        // Create active_metas array - all null initially
        const active_metas = allocator.alloc(?Meta, u.fields.len) catch @panic("OOM");
        @memset(active_metas, null);

        // Find which field has an entity (the active one from initialization)
        for (u.fields, 0..) |field_opt, i| {
            if (field_opt != null) {
                active_metas[i] = Meta{
                    .function = "",
                    .file = loc.file,
                    .line = loc.line,
                    .column = loc.column,
                };
                break;
            }
        }

        u.analyte.variant_safety = .{ .active_metas = active_metas };
    }

    // =========================================================================
    // Aggregate Init Handler
    // =========================================================================

    /// Handle aggregate_init - copy variant_safety state from source unions to struct fields.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy variant_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i >= params.elements.len) break;
                    const src = params.elements[i];
                    copyVariantSafetyState(state, field_gid, src);
                }
            },
            .region => |r| {
                // For arrays/regions: use uniform model - first element applies to all
                if (params.elements.len > 0) {
                    copyVariantSafetyState(state, r.to, params.elements[0]);
                }
            },
            else => {},
        }
    }

    /// Copy variant_safety state from a source to a destination refinement.
    fn copyVariantSafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values don't have variant_safety
            .fnptr => null, // Function pointers don't have variant_safety
        };

        // For interned/fnptr sources, nothing to copy
        if (src_gid == null) return;

        const src_ref = state.refinements.at(src_gid.?);
        const dst_ref = state.refinements.at(dst_gid);

        // Only copy variant_safety for union types
        switch (dst_ref.*) {
            .@"union" => |*u| {
                // Source must also be a union
                if (src_ref.* != .@"union") return;
                const src_u = src_ref.@"union";
                const src_vs = src_u.analyte.variant_safety orelse return;

                // Copy the variant_safety if destination doesn't have one
                if (u.analyte.variant_safety == null) {
                    const allocator = state.refinements.list.allocator;
                    u.analyte.variant_safety = src_vs.copy(allocator) catch @panic("OOM");

                    // Also copy field entities for active variants
                    for (src_vs.active_metas, 0..) |meta_opt, i| {
                        if (meta_opt != null) {
                            // This variant is active - ensure field entity exists
                            if (i < src_u.fields.len and i < u.fields.len) {
                                if (u.fields[i] == null) {
                                    if (src_u.fields[i]) |src_field_gid| {
                                        // Copy the field entity from source
                                        const src_field_ref = state.refinements.at(src_field_gid).*;
                                        const copied_gid = Refinements.Refinement.copyTo(src_field_ref, state.refinements, state.refinements) catch @panic("OOM");
                                        // Re-fetch after potential reallocation
                                        state.refinements.at(dst_gid).@"union".fields[i] = copied_gid;
                                    }
                                }
                            }
                        }
                    }
                }
            },
            .@"struct" => |s| {
                // If destination is a struct, recurse into fields
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyVariantSafetyStateRecursive(state.refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyVariantSafetyStateRecursive(state.refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyVariantSafetyStateRecursive(state.refinements, e.to, src_ref.errorunion.to);
            },
            .region => |r| {
                if (src_ref.* != .region) return;
                copyVariantSafetyStateRecursive(state.refinements, r.to, src_ref.region.to);
            },
            // Other types don't have variant_safety
            else => {},
        }
    }

    /// Recursively copy variant_safety state from source GID to destination GID.
    fn copyVariantSafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        switch (dst_ref.*) {
            .@"union" => |*u| {
                if (src_ref.* != .@"union") return;
                const src_u = src_ref.@"union";
                const src_vs = src_u.analyte.variant_safety orelse return;

                if (u.analyte.variant_safety == null) {
                    const allocator = refinements.list.allocator;
                    u.analyte.variant_safety = src_vs.copy(allocator) catch @panic("OOM");

                    // Also copy field entities for active variants
                    for (src_vs.active_metas, 0..) |meta_opt, i| {
                        if (meta_opt != null) {
                            if (i < src_u.fields.len and i < u.fields.len) {
                                if (u.fields[i] == null) {
                                    if (src_u.fields[i]) |src_field_gid| {
                                        const src_field_ref = refinements.at(src_field_gid).*;
                                        const copied_gid = Refinements.Refinement.copyTo(src_field_ref, refinements, refinements) catch @panic("OOM");
                                        // Re-fetch after potential reallocation
                                        refinements.at(dst_gid).@"union".fields[i] = copied_gid;
                                    }
                                }
                            }
                        }
                    }
                }
            },
            .@"struct" => |s| {
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyVariantSafetyStateRecursive(refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyVariantSafetyStateRecursive(refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyVariantSafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
            },
            .region => |r| {
                if (src_ref.* != .region) return;
                copyVariantSafetyStateRecursive(refinements, r.to, src_ref.region.to);
            },
            else => {},
        }
    }

    // =========================================================================
    // Runtime Call Filter
    // =========================================================================

    /// Runtime call filter for variant safety.
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
        // No variant_safety-specific call intercepts currently needed
        return false;
    }
};
