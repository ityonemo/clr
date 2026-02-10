const std = @import("std");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const Tid = Refinements.Tid;
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;
const tag = @import("../tag.zig");

/// Tracks where a pointer came from for @fieldParentPtr validation.
/// Set by struct_field_ptr to record the origin container type and field.
pub const FieldParentPtrSafety = struct {
    field_index: usize, // Which field of the container
    container_type_id: Tid, // Type id for type identity & name lookup

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(std.mem.asBytes(&self.field_index));
        hasher.update(std.mem.asBytes(&self.container_type_id));
    }

    /// Set fieldparentptr_safety on pointer created by struct_field_ptr.
    /// Records the container type and field index for later validation.
    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        const results = state.results;
        const refinements = state.refinements;

        // Get the pointer we just created
        const ptr_idx = results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // Get the container's type_id from the base pointer
        const base_ref: Gid = switch (params.base) {
            .inst => |inst| results[inst].refinement orelse return,
            .int_var => |ip_idx| refinements.getGlobal(ip_idx) orelse return,
            .int_const => return, // interned constant, can't track
        };
        const base_refinement = refinements.at(base_ref);
        if (base_refinement.* != .pointer) return;

        const container_idx = base_refinement.pointer.to;
        const container = refinements.at(container_idx);

        const container_type_id: Tid = switch (container.*) {
            .@"struct" => |s| s.type_id,
            .@"union" => |u| u.type_id,
            else => return, // Not a struct/union, can't track
        };

        ptr.analyte.fieldparentptr_safety = .{
            .field_index = params.field_index,
            .container_type_id = container_type_id,
        };
    }

    /// Initialize fieldparentptr_safety on a global field pointer.
    /// Called via splatInitGlobal when processing global struct field pointers.
    pub fn init_global(refinements: *Refinements, ptr_gid: Gid, pointee_gid: Gid, ctx: *Context, is_undefined: bool, is_null_opt: bool, loc: tag.GlobalLocation, field_info: ?tag.GlobalFieldInfo) void {
        _ = pointee_gid; // Unused by fieldparentptr_safety
        _ = ctx;
        _ = is_undefined; // Handled by undefined_safety.init_global
        _ = is_null_opt; // Handled by null_safety.init_global
        _ = loc;

        const info = field_info orelse return;

        // Set fieldparentptr_safety on the pointer
        const ptr_ref = refinements.at(ptr_gid);
        if (ptr_ref.* != .pointer) return;

        ptr_ref.pointer.analyte.fieldparentptr_safety = .{
            .field_index = info.field_index,
            .container_type_id = info.container_type_id,
        };
    }

    /// Merge fieldparentptr_safety from branches to original.
    /// If branches have conflicting origins (different field_index or container_type_id),
    /// clear fieldparentptr_safety to indicate ambiguous origin (will error on @fieldParentPtr).
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
        if (orig_ref.* != .pointer) return;

        // Gather fieldparentptr_safety from all branches and detect conflicts
        var first_fps: ?FieldParentPtrSafety = null;
        var has_conflict = false;

        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;

            const branch_ref = branch.refinements.at(branch_gid);
            if (branch_ref.* != .pointer) continue;

            if (branch_ref.pointer.analyte.fieldparentptr_safety) |fps| {
                if (first_fps) |first| {
                    // Check for conflict - different field or container type
                    if (fps.field_index != first.field_index or
                        fps.container_type_id != first.container_type_id)
                    {
                        has_conflict = true;
                    }
                } else {
                    first_fps = fps;
                }
            }
        }

        if (has_conflict) {
            // Clear fieldparentptr_safety to indicate ambiguous origin
            // This will cause @fieldParentPtr to error with "non-field pointer"
            orig_ref.pointer.analyte.fieldparentptr_safety = null;
        } else if (first_fps) |fps| {
            // All branches agree (or only one branch has it) - use that value
            // Only update if original doesn't already have a value
            if (orig_ref.pointer.analyte.fieldparentptr_safety == null) {
                orig_ref.pointer.analyte.fieldparentptr_safety = fps;
            }
        }
    }

    /// Validate @fieldParentPtr usage.
    /// Checks that the field pointer actually came from a struct_field_ptr with matching type and field.
    pub fn field_parent_ptr(state: State, index: usize, params: tag.FieldParentPtr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        // Get the field pointer's refinement GID
        const ptr_idx: Gid = switch (params.field_ptr) {
            .inst => |idx| results[idx].refinement orelse return,
            .int_var => |ip_idx| refinements.getGlobal(ip_idx) orelse return,
            .int_const => return, // interned constant, can't track
        };
        const ptr_ref = refinements.at(ptr_idx);

        // ptr must be a pointer refinement
        if (ptr_ref.* != .pointer) return;

        // Check 1: Pointer must have fieldparentptr_safety (came from struct_field_ptr)
        const origin = ptr_ref.pointer.analyte.fieldparentptr_safety orelse {
            return reportNotFieldPointer(state.ctx, params);
        };

        // Get expected type_id from params.ty (the claimed container type)
        // Note: We use params.ty instead of the result entity because memory_safety
        // may have already updated the result to point to the original container.
        const expected_type_id: Tid = blk: {
            // params.ty is pointer -> struct/union, extract the struct/union type_id
            switch (params.ty.ty) {
                .pointer => |container_ty| break :blk container_ty.id orelse return,
                else => return,
            }
        };

        // Check 2: Container type must match (compare type_ids)
        if (origin.container_type_id != expected_type_id) {
            return reportWrongContainerType(state.ctx, origin, expected_type_id);
        }

        // Check 3: Field index must match
        if (origin.field_index != params.field_index) {
            return reportWrongFieldIndex(state.ctx, origin, params.field_index);
        }
    }

    fn getFieldName(ctx: *Context, type_id: Tid, field_index: usize) []const u8 {
        if (ctx.getFieldId(type_id, @intCast(field_index))) |name_id| {
            return ctx.getName(name_id);
        }
        return "unknown";
    }

    fn reportNotFieldPointer(ctx: *Context, params: tag.FieldParentPtr) !void {
        _ = params;
        try ctx.meta.print(ctx.writer, "fieldParentPtr on non-field pointer in ", .{});
        return error.InvalidFieldParentPtr;
    }

    fn reportWrongContainerType(ctx: *Context, origin: FieldParentPtrSafety, expected_type_id: Tid) !void {
        const origin_field = getFieldName(ctx, origin.container_type_id, origin.field_index);
        const expected_field = getFieldName(ctx, expected_type_id, origin.field_index);
        try ctx.meta.print(ctx.writer, "fieldParentPtr type mismatch in ", .{});
        try ctx.writer.print("pointer is from type {d}.{s}, but @fieldParentPtr claims type {d}.{s}\n", .{
            origin.container_type_id,
            origin_field,
            expected_type_id,
            expected_field,
        });
        return error.FieldParentPtrTypeMismatch;
    }

    fn reportWrongFieldIndex(ctx: *Context, origin: FieldParentPtrSafety, expected_field_index: usize) !void {
        const origin_field = getFieldName(ctx, origin.container_type_id, origin.field_index);
        const expected_field = getFieldName(ctx, origin.container_type_id, expected_field_index);
        try ctx.meta.print(ctx.writer, "fieldParentPtr field mismatch in ", .{});
        try ctx.writer.print("pointer is from field {s}, but @fieldParentPtr claims field {s}\n", .{
            origin_field,
            expected_field,
        });
        return error.FieldParentPtrFieldMismatch;
    }
};

// =============================================================================
// Tests
// =============================================================================

const Inst = @import("../Inst.zig");

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_gid = 0,
    };
}

test "struct_field_ptr records origin on field pointer" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create field entities first
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field1_gid = try refinements.appendEntity(.{ .scalar = .{} });

    // Create a struct with type_id = 100
    // Note: fields array is owned by refinements, freed on deinit
    const fields = try allocator.alloc(Gid, 2);
    fields[0] = field0_gid;
    fields[1] = field1_gid;

    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = fields, .type_id = 100 } });
    const base_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = struct_gid } });

    // Create field pointer (result of struct_field_ptr)
    const field_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = field1_gid } });

    var results = [_]Inst{
        .{ .refinement = base_ptr_gid }, // inst 0: pointer to struct
        .{ .refinement = field_ptr_gid }, // inst 1: result of struct_field_ptr
    };
    const state = testState(&ctx, &results, &refinements);

    // Call struct_field_ptr to set origin tracking
    try FieldParentPtrSafety.struct_field_ptr(state, 1, .{ .base = .{ .inst = 0 }, .field_index = 1, .ty = .{ .ty = .{ .scalar = {} } } });

    // Verify fieldparentptr_safety was set
    const ptr_ref = refinements.at(field_ptr_gid);
    const origin = ptr_ref.pointer.analyte.fieldparentptr_safety.?;
    try std.testing.expectEqual(@as(usize, 1), origin.field_index);
    try std.testing.expectEqual(@as(Tid, 100), origin.container_type_id);
}

test "field_parent_ptr succeeds when origin matches" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create the field pointer with matching origin
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        .analyte = .{ .fieldparentptr_safety = .{ .field_index = 0, .container_type_id = 100 } },
    } });

    // Create expected parent struct type (what fieldParentPtr should return pointer to)
    // Note: fields array is owned by refinements, freed on deinit
    const parent_fields = try allocator.alloc(Gid, 1);
    parent_fields[0] = scalar_gid;
    const parent_struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = parent_fields, .type_id = 100 } });
    const result_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = parent_struct_gid } });

    var results = [_]Inst{
        .{ .refinement = field_ptr_gid }, // inst 0: field pointer
        .{ .refinement = result_ptr_gid }, // inst 1: result of fieldParentPtr (ptr to parent)
    };
    const state = testState(&ctx, &results, &refinements);

    // Should succeed - type_id 100 matches, field_index 0 matches
    try FieldParentPtrSafety.field_parent_ptr(state, 1, .{ .field_ptr = .{ .inst = 0 }, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } });
}

test "field_parent_ptr errors on non-field pointer" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create a regular pointer with NO fieldparentptr_safety
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        // No fieldparentptr_safety set
    } });

    // Create expected parent struct type
    // Note: fields array is owned by refinements, freed on deinit
    const parent_fields = try allocator.alloc(Gid, 1);
    parent_fields[0] = scalar_gid;
    const parent_struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = parent_fields, .type_id = 100 } });
    const result_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = parent_struct_gid } });

    var results = [_]Inst{
        .{ .refinement = ptr_gid }, // inst 0: regular pointer (not from field access)
        .{ .refinement = result_ptr_gid }, // inst 1: result
    };
    const state = testState(&ctx, &results, &refinements);

    // Should error - pointer doesn't have fieldparentptr_safety
    const result = FieldParentPtrSafety.field_parent_ptr(state, 1, .{ .field_ptr = .{ .inst = 0 }, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } });
    try std.testing.expectError(error.InvalidFieldParentPtr, result);
}

test "field_parent_ptr errors on wrong container type" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Field pointer claims to be from type_id 100
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        .analyte = .{ .fieldparentptr_safety = .{ .field_index = 0, .container_type_id = 100 } },
    } });

    // But fieldParentPtr claims type_id 200
    // Note: fields array is owned by refinements, freed on deinit
    const parent_fields = try allocator.alloc(Gid, 1);
    parent_fields[0] = scalar_gid;
    const parent_struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = parent_fields, .type_id = 200 } });
    const result_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = parent_struct_gid } });

    var results = [_]Inst{
        .{ .refinement = field_ptr_gid }, // inst 0: field pointer from type 100
        .{ .refinement = result_ptr_gid }, // inst 1: result claims type 200
    };
    const state = testState(&ctx, &results, &refinements);

    // Should error - type mismatch (100 != 200)
    // params.ty is pointer -> struct with type_id 200 (the claimed type)
    const result = FieldParentPtrSafety.field_parent_ptr(state, 1, .{ .field_ptr = .{ .inst = 0 }, .field_index = 0, .ty = .{ .ty = .{ .pointer = &.{ .id = 200, .ty = .{ .scalar = {} } } } } });
    try std.testing.expectError(error.FieldParentPtrTypeMismatch, result);
}

test "field_parent_ptr errors on wrong field index" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Field pointer is from field 0
    const scalar_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const scalar2_gid = try refinements.appendEntity(.{ .scalar = .{} });
    const field_ptr_gid = try refinements.appendEntity(.{ .pointer = .{
        .to = scalar_gid,
        .analyte = .{ .fieldparentptr_safety = .{ .field_index = 0, .container_type_id = 100 } },
    } });

    // Matching parent type
    // Note: fields array is owned by refinements, freed on deinit
    const parent_fields = try allocator.alloc(Gid, 2);
    parent_fields[0] = scalar_gid;
    parent_fields[1] = scalar2_gid;
    const parent_struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .fields = parent_fields, .type_id = 100 } });
    const result_ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .to = parent_struct_gid } });

    var results = [_]Inst{
        .{ .refinement = field_ptr_gid }, // inst 0: field pointer from field 0
        .{ .refinement = result_ptr_gid }, // inst 1: result
    };
    const state = testState(&ctx, &results, &refinements);

    // Should error - field mismatch (claims field 1, but pointer is from field 0)
    // params.ty is pointer -> struct with type_id 100 (matching type, wrong field)
    const result = FieldParentPtrSafety.field_parent_ptr(state, 1, .{ .field_ptr = .{ .inst = 0 }, .field_index = 1, .ty = .{ .ty = .{ .pointer = &.{ .id = 100, .ty = .{ .scalar = {} } } } } });
    try std.testing.expectError(error.FieldParentPtrFieldMismatch, result);
}
