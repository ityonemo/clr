const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const Context = @import("../Context.zig");
const tag = @import("../tag.zig");
const core = @import("../core.zig");
const Meta = core.Meta;
const State = @import("../lib.zig").State;

/// Validate that variant_safety state is consistent with the refinement.
/// - MUST EXIST: .union
/// - MUST BE NULL: .scalar, .pointer, .optional, .errorunion, .struct, .recursive, .fnptr, .allocator, .region
/// - NO ANALYTE: .void, .noreturn, .unimplemented
pub fn testValid(refinement: Refinements.Refinement) void {
    switch (refinement) {
        // variant_safety must exist on unions
        .@"union" => |u| {
            if (u.analyte.variant_safety) |vs| {
                vs.testValidForUnion(u);
            } else {
                std.debug.panic("variant_safety must be set on unions", .{});
            }
        },
        // variant_safety must be null on non-union types
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
        inline .pointer, .optional, .errorunion, .@"struct", .recursive, .fnptr, .region => |data, t| {
            if (data.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got {s}", .{@tagName(t)});
            }
        },
        .void, .noreturn, .unimplemented => {},
    }
}

/// VariantSafety tracks which union variants are active and where they were set.
/// The active_metas slice has one entry per union field:
/// - null means the field is inactive
/// - non-null Meta means the field is active (with location where it was set)
pub const VariantSafety = struct {
    active_metas: []?Meta,
    undefined_meta: ?Meta = null,
    name_when_set: ?[]const u8 = null,

    /// Deep copy - allocates new slice for active_metas.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        const new_active_metas = try allocator.alloc(?Meta, self.active_metas.len);
        @memcpy(new_active_metas, self.active_metas);
        return .{
            .active_metas = new_active_metas,
            .undefined_meta = self.undefined_meta,
            .name_when_set = self.name_when_set,
        };
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
        hasher.update(&.{@as(u8, if (self.undefined_meta != null) 1 else 0)});
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

    pub fn init(refinements: *Refinements, gid: Gid, ctx: ?*Context, state: tag.InitState) void {
        _ = ctx;
        _ = state;
        initRecursive(refinements, gid);
    }

    fn initRecursive(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .@"union" => |*u| {
                _ = ensureState(refinements, u);
                for (u.fields) |field_opt| {
                    if (field_opt) |field_gid| initRecursive(refinements, field_gid);
                }
            },
            .pointer => |p| initRecursive(refinements, p.to),
            .optional => |o| initRecursive(refinements, o.to),
            .errorunion => |e| initRecursive(refinements, e.to),
            .@"struct" => |s| {
                for (s.fields) |field_gid| initRecursive(refinements, field_gid);
            },
            .region => |r| initRecursive(refinements, r.to),
            .recursive => {},
            .scalar, .allocator, .fnptr, .void, .noreturn, .unimplemented => {},
        }
    }

    fn ensureState(refinements: *Refinements, u: *Refinements.Refinement.Union) *VariantSafety {
        if (u.analyte.variant_safety == null) {
            const active_metas = refinements.list.allocator.alloc(?Meta, u.fields.len) catch @panic("out of memory");
            @memset(active_metas, null);
            u.analyte.variant_safety = .{ .active_metas = active_metas };
        }
        return &u.analyte.variant_safety.?;
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

        const vs = ensureState(refinements, u);
        for (vs.active_metas) |*m| m.* = null;
        vs.active_metas[field_idx] = ctx.meta;
        vs.undefined_meta = null;
        vs.name_when_set = null;
    }

    pub fn union_init(state: State, index: usize, params: tag.UnionInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const ref = state.refinements.at(result_gid);
        if (ref.* != .@"union") return;

        const u = &ref.@"union";
        const vs = ensureState(state.refinements, u);
        if (params.field_index >= vs.active_metas.len) return;
        for (vs.active_metas) |*m| m.* = null;
        vs.active_metas[params.field_index] = state.ctx.meta;
        vs.undefined_meta = null;
        vs.name_when_set = null;
    }

    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const is_undef = switch (params.src) {
            .interned => |interned| interned.ty == .undefined,
            else => false,
        };
        const active_union_field = switch (params.src) {
            .interned => |interned| interned.active_union_field,
            else => null,
        };
        if (!is_undef and active_union_field == null) return;

        const ptr_gid: Gid = switch (params.ptr) {
            .inst => |ptr| state.results[ptr].refinement orelse return,
            .interned => |interned| state.refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return,
        };
        const ptr_ref = state.refinements.at(ptr_gid);
        if (ptr_ref.* != .pointer) return;

        const pointee_gid = ptr_ref.pointer.to;
        const pointee = state.refinements.at(pointee_gid);
        if (pointee.* != .@"union") return;

        const u = &pointee.@"union";
        const vs = ensureState(state.refinements, u);
        if (is_undef) {
            for (vs.active_metas) |*m| m.* = null;
            vs.undefined_meta = state.ctx.meta;
            vs.name_when_set = switch (params.ptr) {
                .inst => |ptr| state.ctx.buildPathName(state.results, state.refinements, ptr),
                .interned, .fnptr => null,
            };
        } else if (active_union_field) |field_index| {
            if (field_index >= vs.active_metas.len) return;
            for (vs.active_metas) |*m| m.* = null;
            vs.active_metas[field_index] = state.ctx.meta;
            vs.undefined_meta = null;
            vs.name_when_set = null;
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
    /// (via valueCopy) get the correct state within this branch.
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
        const vs = ensureState(refinements, u);

        // Check field index bounds
        if (field_index >= u.fields.len) return;

        var has_known_active = false;
        for (vs.active_metas) |m| {
            if (m != null) {
                has_known_active = true;
                break;
            }
        }

        if (!has_known_active and vs.undefined_meta == null) {
            // No known active variant - use the cond_br check to establish it.
            try ensureVariantField(refinements, union_eidx, field_index, union_check.field_type, ctx);

            // Re-fetch union and update local copy's variant_safety
            const u_ref = &refinements.at(union_eidx).@"union";
            const vs_ref = &(u_ref.analyte.variant_safety orelse return);
            for (vs_ref.active_metas) |*meta| {
                meta.* = null;
            }
            vs_ref.active_metas[field_index] = ctx.meta;
            vs_ref.undefined_meta = null;
            vs_ref.name_when_set = null;
        }

        // Also update the loaded source's pointee when available, so compiler-generated
        // tag checks before `u.field = value` establish the field for the following
        // struct_field_ptr on the original pointer.
        const inst_tag = results[union_check.union_inst].inst_tag orelse return;
        if (inst_tag != .load) return;
        const load_src = inst_tag.load.ptr;
        if (has_known_active and load_src != .interned) return;
        const src_ptr_gid: Gid = switch (load_src) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return,
        };

        const source_pointee_gid = refinements.at(src_ptr_gid).pointer.to;
        const source_union_ref = refinements.at(source_pointee_gid);
        if (source_union_ref.* != .@"union") return;

        const source_u = &source_union_ref.@"union";

        // Check field index bounds
        if (field_index >= source_u.fields.len) return;

        try ensureVariantField(refinements, source_pointee_gid, field_index, union_check.field_type, ctx);

        // Re-fetch union and clear all active variants, set only this one
        const source_u_ref = &refinements.at(source_pointee_gid).@"union";
        const source_vs_ref = &(source_u_ref.analyte.variant_safety orelse return);
        for (source_vs_ref.active_metas) |*meta| {
            meta.* = null;
        }
        source_vs_ref.active_metas[field_index] = ctx.meta;
        source_vs_ref.undefined_meta = null;
        source_vs_ref.name_when_set = null;
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
        try updateVariantForUnion(refinements, union_eidx, field_index, union_check.field_type, ctx);

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

        try updateVariantForUnion(refinements, pointee_gid, field_index, union_check.field_type, ctx);
    }

    /// Helper to update variant_safety for a union at the given gid.
    /// Creates field entity if needed, clears all active variants, sets the specified one.
    fn updateVariantForUnion(refinements: *Refinements, union_gid: Gid, field_index: usize, field_type: ?tag.Type, ctx: *Context) !void {
        const union_ref = refinements.at(union_gid);
        if (union_ref.* != .@"union") return;

        const u = &union_ref.@"union";
        const vs = &(u.analyte.variant_safety orelse return);

        // Check field index bounds
        if (field_index >= vs.active_metas.len) return;

        try ensureVariantField(refinements, union_gid, field_index, field_type, ctx);

        // Re-fetch union and update variant_safety
        const u_ref = &refinements.at(union_gid).@"union";
        const vs_ref = &(u_ref.analyte.variant_safety orelse return);

        // Clear all active variants and set only this one
        for (vs_ref.active_metas) |*meta| {
            meta.* = null;
        }
        vs_ref.active_metas[field_index] = ctx.meta;
        vs_ref.undefined_meta = null;
        vs_ref.name_when_set = null;
    }

    /// Ensure the narrowed union variant has a field entity with the real payload
    /// structure and initialized analytes. Silent scalar fallback corrupts analyzer
    /// invariants, so missing type metadata is a codegen bug.
    fn ensureVariantField(refinements: *Refinements, union_gid: Gid, field_index: usize, field_type: ?tag.Type, ctx: *Context) !void {
        const u = refinements.at(union_gid).@"union";
        if (field_index >= u.fields.len) return;
        if (u.fields[field_index] != null) return;

        const ty = field_type orelse @panic("variant narrowing missing union field type");
        if (ty == .unimplemented) @panic("variant narrowing got unimplemented union field type");

        const field_ref = try tag.typeToRefinement(ty, refinements);
        const new_gid = try refinements.appendEntity(field_ref);
        tag.splatInit(refinements, new_gid, ctx, .defined);
        refinements.at(union_gid).@"union".fields[field_index] = new_gid;
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
        var undefined_meta: ?Meta = null;

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

        if (is_undefined) {
            undefined_meta = Meta{
                .function = "",
                .file = loc.file,
                .line = loc.line,
                .column = loc.column,
            };
        }

        u.analyte.variant_safety = .{
            .active_metas = active_metas,
            .undefined_meta = undefined_meta,
        };
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

                const allocator = state.refinements.list.allocator;
                if (u.analyte.variant_safety) |old_vs| old_vs.deinit(allocator);
                u.analyte.variant_safety = src_vs.copy(allocator) catch @panic("OOM");
                const dst_fields_len = u.fields.len;

                // Also copy field entities for active variants.
                for (src_vs.active_metas, 0..) |meta_opt, i| {
                    if (i >= src_u.fields.len or i >= dst_fields_len) continue;
                    if (meta_opt == null) {
                        state.refinements.at(dst_gid).@"union".fields[i] = null;
                        continue;
                    }
                    if (src_u.fields[i]) |src_field_gid| {
                        const src_field_ref = state.refinements.at(src_field_gid).*;
                        const copied_gid = Refinements.Refinement.copyTo(src_field_ref, state.refinements, state.refinements) catch @panic("OOM");
                        state.refinements.at(dst_gid).@"union".fields[i] = copied_gid;
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
