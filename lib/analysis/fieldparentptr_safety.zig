const std = @import("std");
const Refinements = @import("../Refinements.zig");
const Tid = Refinements.Tid;
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;
const tag = @import("../tag.zig");

/// Tracks where a pointer came from for @fieldParentPtr validation.
/// Set by struct_field_ptr to record the origin container type and field.
pub const FieldParentPtrSafety = struct {
    field_index: usize, // Which field of the container
    container_type_id: Tid, // Type id for type identity & name lookup

    /// Set fieldparentptr_safety on pointer created by struct_field_ptr.
    /// Records the container type and field index for later validation.
    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        const results = state.results;
        const refinements = state.refinements;

        // Get the pointer we just created
        const ptr_idx = results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // Get the container's type_id from the base pointer
        const base = params.base orelse return; // interned base, can't track
        const base_ref = results[base].refinement orelse return;
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

    /// Validate @fieldParentPtr usage.
    /// Checks that the field pointer actually came from a struct_field_ptr with matching type and field.
    pub fn field_parent_ptr(state: State, index: usize, params: tag.FieldParentPtr) !void {
        const results = state.results;
        const refinements = state.refinements;

        const field_ptr = params.field_ptr orelse return; // interned ptr, can't track

        const ptr_idx = results[field_ptr].refinement orelse return;
        const ptr_ref = refinements.at(ptr_idx);

        if (ptr_ref.* != .pointer) return;

        // Check 1: Pointer must have fieldparentptr_safety (came from struct_field_ptr)
        const origin = ptr_ref.pointer.analyte.fieldparentptr_safety orelse {
            return reportNotFieldPointer(state.ctx, params);
        };

        // Get expected type_id from the result type (pointer to parent container)
        const expected_type_id: Tid = blk: {
            const result_ref_idx = results[index].refinement orelse return;
            const result_ref = refinements.at(result_ref_idx);
            if (result_ref.* != .pointer) return;
            const container_idx = result_ref.pointer.to;
            const container = refinements.at(container_idx);
            break :blk switch (container.*) {
                .@"struct" => |s| s.type_id,
                .@"union" => |u| u.type_id,
                else => return,
            };
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

    fn reportNotFieldPointer(ctx: *Context, params: tag.FieldParentPtr) !void {
        _ = params;
        try ctx.writer.print("fieldParentPtr on non-field pointer in ", .{});
        try ctx.meta.print(ctx.writer, "", .{});
        try ctx.writer.print("\n", .{});
        return error.InvalidFieldParentPtr;
    }

    fn reportWrongContainerType(ctx: *Context, origin: FieldParentPtrSafety, expected_type_id: Tid) !void {
        try ctx.writer.print("fieldParentPtr type mismatch in ", .{});
        try ctx.meta.print(ctx.writer, "", .{});
        try ctx.writer.print(": pointer is from type {d}, but @fieldParentPtr claims type {d}\n", .{
            origin.container_type_id,
            expected_type_id,
        });
        return error.FieldParentPtrTypeMismatch;
    }

    fn reportWrongFieldIndex(ctx: *Context, origin: FieldParentPtrSafety, expected_field_index: usize) !void {
        try ctx.writer.print("fieldParentPtr field mismatch in ", .{});
        try ctx.meta.print(ctx.writer, "", .{});
        try ctx.writer.print(": pointer is from field {d}, but @fieldParentPtr claims field {d}\n", .{
            origin.field_index,
            expected_field_index,
        });
        return error.FieldParentPtrFieldMismatch;
    }
};
