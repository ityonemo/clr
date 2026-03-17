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

        // Use the type_id from the tag params (set by codegen from the container type)
        // If type_id is 0, we can't track fieldParentPtr
        if (params.type_id == 0) return;

        ptr.analyte.fieldparentptr_safety = .{
            .field_index = params.field_index,
            .container_type_id = params.type_id,
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
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // function pointer, can't track
        };
        const ptr_ref = refinements.at(ptr_idx);

        // ptr must be a pointer refinement
        if (ptr_ref.* != .pointer) return;

        // Check 1: Pointer must have fieldparentptr_safety (came from struct_field_ptr)
        const origin = ptr_ref.pointer.analyte.fieldparentptr_safety orelse {
            return reportNotFieldPointer(state.ctx, params);
        };

        // Check 2: Container type must match (compare type_ids)
        if (origin.container_type_id != params.type_id) {
            return reportWrongContainerType(state.ctx, origin, params.type_id);
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

    // =========================================================================
    // Runtime Call Filter
    // =========================================================================

    /// Runtime call filter for fieldparentptr safety.
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
        // No fieldparentptr_safety-specific call intercepts currently needed
        return false;
    }
};
