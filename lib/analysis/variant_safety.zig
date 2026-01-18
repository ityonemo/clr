const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
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
            if (s.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got scalar", .{});
            }
        },
        .allocator => |a| {
            if (a.analyte.variant_safety != null) {
                std.debug.panic("variant_safety should only exist on unions, got allocator", .{});
            }
        },
        inline .pointer, .optional, .errorunion, .@"struct" => |data, t| {
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
            .int_var => |ip_idx| refinements.getGlobal(ip_idx).?,
            .int_const => return, // comptime constant - no variant tracking
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
            .int_var => |ip_idx| refinements.getGlobal(ip_idx) orelse return,
            .int_const => return, // constant base - no variant checking
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

        const u = &union_ref.@"union";
        const vs = &(u.analyte.variant_safety orelse return);

        // Check field index bounds
        if (union_check.field_index >= u.fields.len) return;

        // Only update variant_safety if no variant is currently known as active.
        // If set_union_tag was called earlier, we already KNOW which variant is active
        // and the cond_br is just a runtime safety check - don't override that knowledge.
        var has_known_active = false;
        for (vs.active_metas) |m| {
            if (m != null) {
                has_known_active = true;
                break;
            }
        }

        if (!has_known_active) {
            // No known active variant - use the cond_br check to establish it.
            // If the local field entity is null, create it.
            if (u.fields[union_check.field_index] == null) {
                u.fields[union_check.field_index] = refinements.appendEntity(.{ .scalar = .{
                    .analyte = .{ .undefined = .{ .defined = {} } },
                } }) catch return;
            }

            // Update local copy's variant_safety
            for (vs.active_metas) |*meta| {
                meta.* = null;
            }
            vs.active_metas[union_check.field_index] = ctx.meta;
        }

        // For globals: also update the global's variant_safety so subsequent loads see it
        const inst_tag = results[union_check.union_inst].inst_tag orelse return;
        if (inst_tag != .load) return;
        const load_src = inst_tag.load.ptr;
        if (load_src != .int_var) return; // Not a global
        const global_ip_idx = load_src.int_var;

        const global_ptr_gid = refinements.getGlobal(global_ip_idx) orelse return;
        const global_pointee_gid = refinements.at(global_ptr_gid).pointer.to;
        const global_union_ref = refinements.at(global_pointee_gid);
        if (global_union_ref.* != .@"union") return;

        const global_u = &global_union_ref.@"union";
        const global_vs = &(global_u.analyte.variant_safety orelse return);

        // Check field index bounds
        if (union_check.field_index >= global_u.fields.len) return;

        // For globals, we always update based on the cond_br check.
        // Unlike locals where set_union_tag in the same function gives us accurate info,
        // globals may have been modified by other functions. The cond_br check is our
        // only source of truth in this function's analysis context.
        // If the global's field entity is null, create it.
        if (global_u.fields[union_check.field_index] == null) {
            global_u.fields[union_check.field_index] = refinements.appendEntity(.{ .scalar = .{
                .analyte = .{ .undefined = .{ .defined = {} } },
            } }) catch return;
        }

        for (global_vs.active_metas) |*meta| {
            meta.* = null;
        }
        global_vs.active_metas[union_check.field_index] = ctx.meta;
    }

    /// Handle switch_br - when switching on a union tag, update the active variant.
    /// In each switch case, we know which variant is active based on the case's items.
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
        const u = &union_ref.@"union";

        // Clear all active variants and set only this one
        const vs = &(u.analyte.variant_safety orelse return);
        for (vs.active_metas) |*meta| {
            meta.* = null;
        }

        // Set this variant as active
        const field_index = union_check.field_index;
        if (field_index < vs.active_metas.len) {
            // Create field entity if it doesn't exist (needed for global unions
            // where set_union_tag may have nulled the field)
            if (field_index < u.fields.len and u.fields[field_index] == null) {
                u.fields[field_index] = refinements.appendEntity(.{ .scalar = .{
                    .analyte = .{ .undefined = .{ .defined = {} } },
                } }) catch return;
            }
            vs.active_metas[field_index] = ctx.meta;
        }
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
        const orig_vs = &u.analyte.variant_safety.?;

        // Merge: mark as active any variant that was active in ANY reachable branch
        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;
            const branch_ref = branch.refinements.at(branch_gid);
            if (branch_ref.* != .@"union") continue;

            const branch_vs = branch_ref.@"union".analyte.variant_safety orelse continue;
            // Skip if different lengths (shouldn't happen for same union type)
            if (orig_vs.active_metas.len != branch_vs.active_metas.len) continue;
            for (orig_vs.active_metas, branch_vs.active_metas, 0..) |*orig_m, branch_m, i| {
                if (branch_m != null) {
                    orig_m.* = branch_m;
                    // Ensure field entity exists - copy from branch if original is null
                    if (u.fields[i] == null) {
                        const branch_u = branch_ref.@"union";
                        if (branch_u.fields[i]) |branch_field_gid| {
                            // Cross-table copy: properly copies pointer .to fields
                            const branch_field_ref = branch.refinements.at(branch_field_gid).*;
                            u.fields[i] = Refinements.Refinement.copyTo(branch_field_ref, branch.refinements, refinements) catch @panic("out of memory");
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
};

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

test "set_union_tag sets active variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create a union with 3 fields (null entities initially)
    // Note: fields array is owned by refinements, freed on deinit
    const fields = try allocator.alloc(?Gid, 3);
    for (fields) |*f| f.* = null;

    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{ .fields = fields, .type_id = 0 } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = union_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Set variant 1 as active
    try VariantSafety.set_union_tag(state, 1, .{ .ptr = .{ .inst = 0 }, .field_index = 1, .ty = .{ .ty = .{ .scalar = {} } } });

    // Check variant_safety was created
    const u = refinements.at(union_eidx).@"union";
    const vs = u.analyte.variant_safety.?;
    try std.testing.expect(vs.active_metas[0] == null);
    try std.testing.expect(vs.active_metas[1] != null);
    try std.testing.expect(vs.active_metas[2] == null);
}

test "struct_field_ptr allows access to active variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create variant_safety with field 1 active
    // Note: active_metas and fields are owned by refinements, freed on deinit
    const active_metas = try allocator.alloc(?Meta, 3);
    for (active_metas) |*m| m.* = null;
    active_metas[1] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };

    // Create fields array - only field 1 needs an entity
    const fields = try allocator.alloc(?Gid, 3);
    for (fields) |*f| f.* = null;
    fields[1] = try refinements.appendEntity(.{ .scalar = .{} });

    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = union_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Access active field 1 - should succeed
    try VariantSafety.struct_field_ptr(state, 1, .{ .base = .{ .inst = 0 }, .field_index = 1, .ty = .{ .ty = .{ .scalar = {} } } });
}

test "struct_field_ptr errors on inactive variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create variant_safety with field 1 active
    // Note: active_metas and fields are owned by refinements, freed on deinit
    const active_metas = try allocator.alloc(?Meta, 3);
    for (active_metas) |*m| m.* = null;
    active_metas[1] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };

    // Create fields array
    const fields = try allocator.alloc(?Gid, 3);
    for (fields) |*f| f.* = null;
    fields[1] = try refinements.appendEntity(.{ .scalar = .{} });

    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = union_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Access inactive field 0 - should error
    const result = VariantSafety.struct_field_ptr(state, 1, .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.InactiveVariantAccess, result);
}

test "struct_field_val errors on inactive variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create variant_safety with field 2 active
    // Note: active_metas and fields are owned by refinements, freed on deinit
    const active_metas = try allocator.alloc(?Meta, 3);
    for (active_metas) |*m| m.* = null;
    active_metas[2] = .{ .function = "test", .file = "test.zig", .line = 1, .column = null };

    // Create fields array
    const fields = try allocator.alloc(?Gid, 3);
    for (fields) |*f| f.* = null;
    fields[2] = try refinements.appendEntity(.{ .scalar = .{} });

    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
        .fields = fields,
        .type_id = 0,
    } });

    var results = [_]Inst{.{ .refinement = union_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Access inactive field 0 - should error
    const result = VariantSafety.struct_field_val(state, 1, .{ .operand = 0, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.InactiveVariantAccess, result);
}

test "struct_field_ptr errors on ambiguous variant after merge" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create variant_safety with TWO fields active (ambiguous state after merge)
    // Note: active_metas and fields are owned by refinements, freed on deinit
    const active_metas = try allocator.alloc(?Meta, 3);
    for (active_metas) |*m| m.* = null;
    active_metas[0] = .{ .function = "branch1", .file = "test.zig", .line = 1, .column = null };
    active_metas[1] = .{ .function = "branch2", .file = "test.zig", .line = 2, .column = null };

    // Create fields array
    const fields = try allocator.alloc(?Gid, 3);
    for (fields) |*f| f.* = null;
    fields[0] = try refinements.appendEntity(.{ .scalar = .{} });
    fields[1] = try refinements.appendEntity(.{ .scalar = .{} });

    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{ .variant_safety = .{ .active_metas = active_metas } },
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .to = union_eidx } });

    var results = [_]Inst{.{ .refinement = ptr_eidx }} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Access any field when ambiguous - should error
    const result = VariantSafety.struct_field_ptr(state, 1, .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.AmbiguousVariantAccess, result);
}

test "cond_br is no-op" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // cond_br should succeed without modifying anything
    try VariantSafety.cond_br(state, 1, .{ .branch = true, .condition_idx = 0 });
}
