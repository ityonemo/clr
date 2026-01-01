// =============================================================================
// IMPORTANT: Tag handlers must NOT reach into analyte fields directly.
// Analyte manipulation (undefined, memory_safety) is the responsibility of
// analysis modules in lib/analysis/. Tag handlers should only:
//   1. Set up refinement STRUCTURE (pointer, scalar, struct, etc.)
//   2. Call splat() to dispatch to analysis modules
//   3. Use generic helpers like copyAnalyteState() that work on any analyte
// If you find yourself writing `.analyte.undefined = ...` in a tag handler,
// that logic belongs in an analysis module instead.
// =============================================================================
//
// =============================================================================
// ENTITY OPERATION CLASSIFICATION
// =============================================================================
//
// Each tag performs one of these entity operations:
//
// CREATE - Allocate new independent entity
//   Tags: alloc, alloc_create, arg, bitcast (on value), bit_and, cmp_*,
//         ctz, sub, add_with_overflow, sub_with_overflow, is_non_err,
//         is_non_null, is_null, get_union_tag, unwrap_errunion_err,
//         wrap_errunion_err, wrap_errunion_payload, block, br, cond_br,
//         intcast, ptr_add, array_to_slice, slice, slice_len, aggregate_init
//
// CREATE-SEMIDEEP - Create new entity with semideep copy from source
//   (Copy values recursively, but pointers reference same target)
//   Tags: load, union_init, optional_payload, unwrap_errunion_payload,
//         struct_field_val
//
// CREATE-THAT-REFERENCES - Create new pointer entity referencing existing entity
//   Tags: struct_field_ptr, bitcast (on pointer), optional_payload_ptr (TODO),
//         unwrap_errunion_payload_ptr (TODO)
//
// MODIFY - Update pre-existing entity
//   Tags: store, store_safe, ret_safe, ret_load, ret_ptr, set_union_tag,
//         alloc_destroy, memset_safe
//
// NO-OP / METADATA - No entity impact
//   Tags: noop, noop_pruned_debug, dbg_stmt, dbg_var_ptr, dbg_var_val,
//         dbg_arg_inline, dbg_inline_block, unreach, ret_addr,
//         stack_trace_frames, @"try" (TODO)
//
// =============================================================================

const std = @import("std");
const Inst = @import("Inst.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const EIdx = Inst.EIdx;
const Context = @import("Context.zig");
const State = Inst.State;
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
const NullSafety = @import("analysis/null_safety.zig").NullSafety;
const VariantSafety = @import("analysis/variant_safety.zig").VariantSafety;
const FieldParentPtrSafety = @import("analysis/fieldparentptr_safety.zig").FieldParentPtrSafety;
pub const analyses = .{ Undefined, MemorySafety, NullSafety, VariantSafety, FieldParentPtrSafety };

// Type

pub const Name = u32;

/// Represents a struct or union field with type and optional name.
/// 
/// This struct exists to propagate information from the AIR generator into
/// the parameters of a instruction.  Some operations are expected to set
/// the type based on interned information; in those cases, the type will be used.
pub const Type = struct {
    id: ?Name,
    ty: union(enum) {
        scalar: void,
        pointer: *const Type,
        optional: *const Type,
        errorunion: *const Type, // error union payload type
        null: *const Type, // used to signal that an optional is being set to null.  Inner type must be optional.
        undefined: *const Type, // used to signal that the type is undefined, must be scalar, pointer, optional
        region: *const Type, // unused, for now, will represent slices (maybe)
        @"struct": []const Type, // field types for struct
        @"union": []const Type, // field types for union
        void: void,
    }
};

/// Source reference for instructions - indicates where a value comes from.
/// Used by store, br, ret_safe, and other tags that reference source values,
/// as well as by calls.
pub const Src = union(enum) {
    /// Runtime value from a result in the results table
    eidx: usize,
    /// Compile-time known value (interned in InternPool)
    interned: Type,
    /// Other sources (globals, etc.) - currently unimplemented
    other: void,
};

/// Convert a Type (from codegen) to a Refinement.
/// Used when storing interned values to determine the refinement structure.
/// Note: .null types are converted to .optional refinements since null is a valid defined value.
pub fn typeToRefinement(ty: Type, refinements: *Refinements) !Refinement {
    const type_id = ty.id orelse 0;
    return switch (ty.ty) {
        .scalar => .{ .scalar = .{ .analyte = .{}, .type_id = type_id } },
        .void => .void,
        .pointer => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .pointer = .{ .analyte = .{}, .type_id = type_id, .to = child_idx } };
        },
        .optional => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .analyte = .{}, .type_id = type_id, .to = child_idx } };
        },
        .errorunion => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .errorunion = .{ .analyte = .{}, .type_id = type_id, .to = child_idx } };
        },
        .null => |child| {
            // .null is a null optional value - creates same structure as .optional
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .analyte = .{}, .type_id = type_id, .to = child_idx } };
        },
        .undefined => |child| {
            // .undefined wraps a type - recurse into inner type.
            // The undefined.store handler checks for .undefined wrapper and marks as undefined.
            return typeToRefinement(child.*, refinements);
        },
        .region => @panic("regions not implemented yet"),
        inline .@"struct", .@"union" => |type_fields, class_tag| blk: {
            const allocator = refinements.list.allocator;
            const FieldType = if (class_tag == .@"union") ?Refinements.EIdx else Refinements.EIdx;
            const fields = allocator.alloc(FieldType, type_fields.len) catch @panic("out of memory");
            for (type_fields, 0..) |field_type, i| {
                // For structs, create entities for each field; for unions, all fields start inactive (null)
                fields[i] = if (class_tag == .@"union")
                    null
                else
                    try refinements.appendEntity(try typeToRefinement(field_type, refinements));
            }
            break :blk @unionInit(Refinement, @tagName(class_tag), .{ .fields = fields, .type_id = type_id });
        },
    };
}

// Tag payload types

/// Entity operation: CREATE
/// Creates pointer entity + pointee entity
pub const Alloc = struct {
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create pointee from type info
        const pointee_ref = try typeToRefinement(self.ty, state.refinements);
        const pointee_idx = try state.refinements.appendEntity(pointee_ref);
        // Create pointer entity pointing to the typed pointee
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = pointee_idx } });
        try splat(.alloc, state, index, self);
    }
};

pub const AllocCreate = struct {
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // AllocCreate returns Allocator.Error!*T, so we create errorunion -> ptr -> T
        // Create pointee from type info
        const pointee_ref = try typeToRefinement(self.ty, state.refinements);
        const pointee_idx = try state.refinements.appendEntity(pointee_ref);
        // Create pointer entity pointing to the typed pointee
        const ptr_idx = try state.refinements.appendEntity(.{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = pointee_idx } });
        // Wrap in error union
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .errorunion = .{ .analyte = .{}, .type_id = 0, .to = ptr_idx } });
        try splat(.alloc_create, state, index, self);
    }
};

pub const AllocDestroy = struct {
    /// Index into results[] array for the pointer being freed
    ptr: usize,
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        try splat(.alloc_destroy, state, index, self);
    }
};

/// Entity operation: CREATE
/// Creates new entity for the argument value (via cross-table copyTo).
///
/// Arg copies caller's entity into local refinements directly - NO pointer wrapper.
///
/// AIR Semantics:
/// - AIR arg instructions contain VALUES directly, not pointers to stack locations.
/// - If the parameter type is `*u8`, the instruction contains the pointer value.
/// - If the parameter type is `u8`, the instruction contains the scalar value.
/// - Taking `&param` in source code generates explicit `alloc` + `store_safe` in AIR.
///
/// Interprocedural Analysis:
/// - We deep-copy the caller's entity into local refinements to avoid cross-function aliasing.
/// - caller_ref enables backward propagation on function close (backPropagate):
///   for pointer args, updates through the pointer in the callee are propagated
///   back to the caller's pointee entity.
///
/// Example for `fn set_value(ptr: *u8) { ptr.* = 5; }`:
/// - Caller passes pointer entity P1 -> scalar S1 (undefined)
/// - Arg copies to P1' -> S1' in local refinements
/// - store_safe(ptr=0) follows P1' to S1', marks S1' as defined
/// - backPropagate: propagates S1'.undefined back to S1.undefined
pub const Arg = struct {
    name_id: u32, // Parameter name ID, resolved via ctx.getName()
    /// Source of the argument value - either runtime (.eidx) or compile-time (.interned)
    value: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.value) {
            .eidx => |eidx| {
                const caller_eidx: EIdx = @intCast(eidx);
                const cp = state.caller_refinements orelse unreachable; // Entrypoint shouldn't have args
                // Copy caller's entity directly (no pointer wrapping - AIR args contain values)
                const local_copy_idx = try cp.at(caller_eidx).*.copyTo(cp, state.refinements);
                state.results[index].refinement = local_copy_idx;
                // Set caller_eidx for backward propagation on function close
                state.results[index].caller_eidx = caller_eidx;
                // name_id is in inst_tag (stored by Inst.apply)
            },
            .interned => |ty| {
                // Compile-time constant - create entity from type info
                const ref = try typeToRefinement(ty, state.refinements);
                const local_idx = try state.refinements.appendEntity(ref);
                state.results[index].refinement = local_idx;
                // No backward propagation needed for constants (caller_eidx stays null)
                // name_id is in inst_tag (stored by Inst.apply)
            },
            .other => @panic("Arg: .other source not yet implemented"),
        }
        try splat(.arg, state, index, self);
    }
};

/// Entity operation: CREATE (on value) or CREATE-THAT-REFERENCES (on pointer)
/// For pointers: creates new pointer entity referencing same pointee.
/// For values: creates new entity.
pub const Bitcast = struct {
    /// Source value being bitcast.
    src: Src,
    /// Destination type - available for analysis handlers.
    /// NOTE: Optional pointers (?*T) are the ONLY optional type with the same
    /// bit width as their non-optional form (null = address 0). This is why
    /// bitcast can convert *T to ?*T without changing the value.
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.src) {
            .eidx => |src| {
                const src_eidx = state.results[src].refinement orelse return;
                const src_ref = state.refinements.at(src_eidx);

                // Check if we're converting pointer to optional (e.g., *T to ?*T)
                if (self.ty.ty == .optional and src_ref.* == .pointer) {
                    // Create optional wrapper that points to the existing pointer entity
                    const opt_idx = try state.refinements.appendEntity(.{
                        .optional = .{ .analyte = .{}, .type_id = 0, .to = src_eidx },
                    });
                    state.results[index].refinement = opt_idx;
                } else {
                    // Default: share the source refinement directly
                    state.results[index].refinement = src_eidx;
                }
            },
            .interned, .other => {},
        }

        try splat(.bitcast, state, index, self);
    }
};

/// Block creates a labeled scope that can be the target of `br` (break) instructions.
/// The block's result type is known from codegen; br instructions may override with actual value.
pub const Block = struct {
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create block result from type info
        const result_ref = try typeToRefinement(self.ty, state.refinements);
        _ = try Inst.clobberInst(state.refinements, state.results, index, result_ref);
        try splat(.block, state, index, self);
    }
};

/// Br (break) transfers control to a block, optionally carrying a value.
/// - `break :blk value` → src is .eidx or .interned with the value
/// - `break :blk` (void) → src is .interned with .void type
pub const Br = struct {
    block: usize,
    /// Value being passed to the block.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // For eidx sources, share the source's refinement with the block.
        // This ensures operations on the block (like alloc_destroy) affect the same entity.
        // Clone preserves indices, so sharing propagates correctly through merge.
        switch (self.src) {
            .eidx => |src| {
                const src_idx = state.results[src].refinement orelse return;
                state.results[self.block].refinement = src_idx;
            },
            // Interned values are handled by analysis modules (e.g., undefined.br marks as defined)
            .interned, .other => {},
        }
        try splat(.br, state, index, self);
    }

    /// Copy analysis state (memory_safety, undefined) from source entity to destination.
    /// This preserves the destination's type structure while updating its analysis state.
    fn copyAnalyteState(refinements: *Refinements, dst_idx: EIdx, src_idx: EIdx) void {
        const dst = refinements.at(dst_idx);
        const src = refinements.at(src_idx);

        switch (dst.*) {
            .pointer => |*dp| {
                if (src.* == .pointer) {
                    const sp = src.pointer;
                    // Copy analyte (memory_safety, undefined)
                    dp.analyte = sp.analyte;
                    // Recursively copy pointee's analysis state
                    copyAnalyteState(refinements, dp.to, sp.to);
                }
            },
            .scalar => |*ds| {
                if (src.* == .scalar) {
                    ds.* = src.scalar;
                }
            },
            .optional => |*d_opt| {
                if (src.* == .optional) {
                    copyAnalyteState(refinements, d_opt.to, src.optional.to);
                }
            },
            .@"struct" => |*d_struct| {
                if (src.* == .@"struct") {
                    const s_struct = src.@"struct";
                    for (d_struct.fields, s_struct.fields) |df, sf| {
                        copyAnalyteState(refinements, df, sf);
                    }
                }
            },
            else => {},
        }
    }
};

// DbgStmt is special: it updates the context's meta line/column for error reporting.
// It does NOT splat because no analysis needs to track debug statements.
pub const DbgStmt = struct {
    line: u32,
    column: u32,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        state.ctx.meta.line = state.ctx.base_line + self.line + 1;
        state.ctx.meta.column = self.column;
    }
};

/// Params passed to analysis handlers for dbg_var_ptr.
pub const DbgVarPtrParams = struct {
    ptr: ?usize,
    name_id: u32,
};

/// DbgVarPtr associates a variable name with a pointer instruction.
/// Sets the name on the instruction for use in error messages.
///
/// NOTE: We still call splat() here because in AIR, dbg_var_ptr comes AFTER
/// the initial store instruction. Analysis modules need to retroactively update
/// their states with the variable name (e.g., undefined.setNameOnUndefined sets
/// name_when_set, memory_safety updates stack_ptr.name). Without this, error
/// messages would lack variable names.
///
/// TODO: dbg_var_val may need similar handling if it affects analysis states.
/// See LIMITATIONS.md for details.
pub const DbgVarPtr = struct {
    /// Index into results[] array for the pointer. Null when the pointer is comptime.
    ptr: ?usize,
    /// Name ID for the variable (looked up via ctx.getName)
    name_id: u32,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        const inst = self.ptr orelse return;
        state.results[inst].name_id = self.name_id;
        try splat(.dbg_var_ptr, state, index, DbgVarPtrParams{ .ptr = self.ptr, .name_id = self.name_id });
    }
};

/// DbgVarVal associates a variable name with a value (non-pointer) instruction.
/// Sets the name on the instruction for use in error messages.
pub const DbgVarVal = struct {
    /// Index into results[] array for the value. Null when the value is comptime.
    ptr: ?usize,
    /// Name ID for the variable (looked up via ctx.getName)
    name_id: u32,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        const inst = self.ptr orelse return;
        state.results[inst].name_id = self.name_id;
    }
};

/// Entity operation: CREATE-SEMIDEEP
/// Creates new entity via semideep copy from pointee.
///
/// Load dereferences a pointer and produces the value stored at that memory location.
/// Uses semideep copy semantics: recursively copies values, but pointer targets are shared.
pub const Load = struct {
    /// Index into results[] array. Null when loading from a global or constant
    /// pointer (interned value with no instruction to trace).
    ptr: ?usize,
    /// Type of the loaded value (from AIR ty_op)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const ptr = self.ptr orelse {
            // Interned/global pointer - use type info to create proper structure
            _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
            try splat(.load, state, index, self);
            return;
        };

        const ptr_ref = state.results[ptr].refinement orelse {
            // Pointer has no refinement - use type info to create proper structure
            _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
            // Path derived from inst_tag.load.ptr at error time
            try splat(.load, state, index, self);
            return;
        };

        // Follow pointer to get pointee
        const pointee_idx = switch (state.refinements.at(ptr_ref).*) {
            .pointer => |p| p.to,
            else => {
                _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
                // Path derived from inst_tag.load.ptr at error time
                try splat(.load, state, index, self);
                return;
            },
        };

        // Semideep copy: create new entity copying values, but pointers reference same target
        const new_idx = try state.refinements.semideepCopy(pointee_idx);
        state.results[index].refinement = new_idx;
        // Path derived from inst_tag.load.ptr at error time
        try splat(.load, state, index, self);
    }
};

/// Entity operation: CREATE-THAT-REFERENCES
/// Creates new pointer entity referencing existing field entity.
///
/// StructFieldPtr gets a pointer to a specific field within a struct or union.
/// Used by struct_field_ptr_index_N instructions.
/// Note: Zig AIR uses this for both struct and union field access.
pub const StructFieldPtr = struct {
    /// Base pointer to the struct or union
    base: ?usize,
    /// Index of the field to access
    field_index: usize,
    /// Result type (pointer to field type)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const base = self.base orelse {
            // Interned/global base - use type info to create proper structure
            _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
            try splat(.struct_field_ptr, state, index, self);
            return;
        };

        // base produces pointer refinement, follow to struct or union
        const base_ref = state.results[base].refinement orelse {
            // Base has no refinement - use type info
            _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
            try splat(.struct_field_ptr, state, index, self);
            return;
        };
        const base_refinement = state.refinements.at(base_ref).*;
        const container_idx = base_refinement.pointer.to;
        const container = state.refinements.at(container_idx).*;

        switch (container) {
            inline .@"struct", .@"union" => |data, tag| {
                // Get field index - for unions, create entity for inactive fields
                // (variant_safety analysis will report error via splat)
                const field_idx = idx: {
                    if (tag == .@"union") {
                        if (data.fields[self.field_index]) |idx| break :idx idx;
                        // Inactive field - create a fresh entity for structure
                        const new_field_ref = try typeToRefinement(self.ty.ty.pointer.*, state.refinements);
                        const new_idx = try state.refinements.appendEntity(new_field_ref);
                        break :idx new_idx;
                    } else {
                        break :idx data.fields[self.field_index];
                    }
                };
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = field_idx } });
                // Path derived from inst_tag (has base and field_index) at error time
            },
            else => |t| std.debug.panic("struct_field_ptr: expected struct or union, got {s}", .{@tagName(t)}),
        }

        try splat(.struct_field_ptr, state, index, self);
    }
};

/// Entity operation: CREATE-SEMIDEEP
/// Creates new entity via semideep copy from field.
///
/// StructFieldVal extracts a field value from a struct or union by value.
/// This is used when accessing `s.field` where `s` is a struct/union value (not a pointer).
/// Note: Zig AIR uses this for both struct and union field access.
pub const StructFieldVal = struct {
    /// Operand containing the struct or union value
    operand: ?usize,
    /// Index of the field to extract
    field_index: usize,
    /// Result type (the field's type)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const operand = self.operand orelse {
            // Interned/global struct/union - use type info to create proper structure
            _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
            try splat(.struct_field_val, state, index, self);
            return;
        };

        const container_ref = state.results[operand].refinement orelse {
            std.debug.panic("struct_field_val: operand instruction {d} has no refinement", .{operand});
        };

        // Get the struct or union and find the field
        switch (state.refinements.at(container_ref).*) {
            inline .@"struct", .@"union" => |data, tag| {
                // Get field index - for unions, create entity for inactive fields
                // (variant_safety analysis will report error via splat)
                const field_idx = idx: {
                    if (tag == .@"union") {
                        if (data.fields[self.field_index]) |idx| break :idx idx;
                        // Inactive field - create a fresh entity for structure
                        const new_field_ref = try typeToRefinement(self.ty, state.refinements);
                        const new_idx = try state.refinements.appendEntity(new_field_ref);
                        break :idx new_idx;
                    } else {
                        break :idx data.fields[self.field_index];
                    }
                };

                // Share the field entity (reading a field doesn't copy it)
                state.results[index].refinement = field_idx;
                // Path derived from inst_tag (has operand and field_index) at error time
            },
            else => |t| std.debug.panic("struct_field_val: expected struct or union, got {s}", .{@tagName(t)}),
        }
        try splat(.struct_field_val, state, index, self);
    }
};

/// Entity operation: CREATE-THAT-REFERENCES
/// Creates new pointer entity referencing parent container.
///
/// FieldParentPtr gets a pointer to the parent container from a field pointer.
/// This is the inverse of struct_field_ptr - given a pointer to a field, it
/// computes a pointer to the containing struct/union.
/// Used by @fieldParentPtr builtin.
pub const FieldParentPtr = struct {
    /// Pointer to the field
    field_ptr: ?usize,
    /// Index of the field within the container
    field_index: usize,
    /// Result type (pointer to parent container)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create pointer to parent container using type info
        _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
        try splat(.field_parent_ptr, state, index, self);
    }
};

/// Entity operation: CREATE-SEMIDEEP
/// Creates new entity via semideep copy from optional payload.
///
/// OptionalPayload extracts the inner value from an optional (e.g., `x.?` or `x orelse`).
/// Uses semideep copy semantics: recursively copies values, but pointer targets are shared.
pub const OptionalPayload = struct {
    /// Source optional being unwrapped.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.src) {
            .eidx => |src| {
                const src_ref_idx = state.results[src].refinement orelse return;
                const src_ref = state.refinements.at(src_ref_idx);

                if (src_ref.* == .optional) {
                    // Extract the payload from the optional (optional.to is the inner value)
                    state.results[index].refinement = src_ref.optional.to;
                } else {
                    // Not an optional - might be error union or direct pass-through
                    state.results[index].refinement = src_ref_idx;
                }
            },
            .interned, .other => {
                // Comptime/global source - create fresh scalar
                // TODO: use type info from .interned to determine structure
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
            },
        }
        try splat(.optional_payload, state, index, self);
    }
};

/// Entity operation: MODIFY
/// Modifies the retval entity with the return value's state.
///
/// RetSafe returns a value from a function. "Safe" refers to safety-checked returns
/// (as opposed to naked/inline assembly returns).
///
/// This handles copying the return value's entity back to the caller's refinements,
/// enabling interprocedural tracking of returned values.
pub const RetSafe = struct {
    /// Value being returned. Use .interned with .void for void returns.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);

        // Copy return value to caller's refinements
        if (state.caller_refinements) |caller_refinements| {
            const return_eidx = state.return_eidx;
            switch (self.src) {
                .eidx => |src| {
                    const src_idx = state.results[src].refinement orelse @panic("return function requested uninitialized instruction value");
                    if (state.refinements.at(src_idx).* == .retval_future) @panic("cannot return an unset_retval");
                    switch (caller_refinements.at(return_eidx).*) {
                        .retval_future, .noreturn => {
                            // .noreturn can be overwritten - it's from an error path return
                            // Copy return value from callee to caller's return slot
                            const new_idx = try state.refinements.at(src_idx).*.copyTo(state.refinements, caller_refinements);
                            // Deep copy for structs to avoid double-free on deinit
                            const new_val = caller_refinements.at(new_idx).*;
                            caller_refinements.at(return_eidx).* = switch (new_val) {
                                .@"struct" => |s| blk: {
                                    const allocator = caller_refinements.list.allocator;
                                    const new_fields = allocator.alloc(Refinements.EIdx, s.fields.len) catch @panic("out of memory");
                                    @memcpy(new_fields, s.fields);
                                    break :blk .{ .@"struct" = .{ .analyte = s.analyte, .fields = new_fields, .type_id = s.type_id } };
                                },
                                else => new_val,
                            };
                        },
                        else => {
                            // Multiple returns within same branch - need to merge
                            // TODO: implement proper merge of return value analysis states
                            @panic("ret_safe: multiple returns in same branch - merge not yet implemented");
                        },
                    }
                },
                .interned => |ty| {
                    // Comptime return value
                    if (ty.ty == .void) {
                        // Void return
                        switch (caller_refinements.at(return_eidx).*) {
                            .retval_future => caller_refinements.at(return_eidx).* = .void,
                            .void => {},
                            else => @panic("void function retval incorrectly set to some value"),
                        }
                    } else {
                        // Non-void comptime value - create structure from type info
                        switch (caller_refinements.at(return_eidx).*) {
                            .retval_future, .void => {
                                caller_refinements.at(return_eidx).* = try typeToRefinement(ty, caller_refinements);
                            },
                            else => {
                                // Multiple returns within same branch - need to merge
                                // TODO: implement proper merge of return value analysis states
                                @panic("ret_safe: multiple interned returns in same branch - merge not yet implemented");
                            },
                        }
                    }
                },
                .other => {
                    // Global/other source - not yet supported
                    @panic("ret_safe: .other source not yet implemented");
                },
            }
        }

        // Splat runs last - analyses see state after copy is complete
        try splat(.ret_safe, state, index, self);
    }
};

/// RetPtr returns a pointer to the return value storage.
/// Used for large return values (structs, unions) where caller provides storage.
///
/// Flow:
/// 1. ret_ptr creates a local entity (the return value) and a pointer to it
/// 2. Operations (store_safe, struct_field_ptr, etc.) modify through the pointer
/// 3. ret_load copies the entity to caller's return slot
pub const RetPtr = struct {
    /// The return value type (pointee type, not pointer type)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create a local entity for the return value based on type
        const return_ref = try typeToRefinement(self.ty, state.refinements);
        const return_idx = try state.refinements.appendEntity(return_ref);

        // Create a pointer to this entity as the result
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = return_idx } });

        try splat(.ret_ptr, state, index, self);
    }
};

/// RetLoad loads from ret_ptr to complete the return.
/// Copies the return value entity to caller's return slot.
pub const RetLoad = struct {
    /// The ret_ptr instruction index
    ptr: usize,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);

        // Get the entity from ret_ptr's pointer
        const ptr_ref = state.results[self.ptr].refinement orelse @panic("ret_load: ret_ptr has no refinement");
        const pointee_idx = state.refinements.at(ptr_ref).pointer.to;

        // Copy to caller's return slot
        if (state.caller_refinements) |caller_refinements| {
            const return_eidx = state.return_eidx;
            switch (caller_refinements.at(return_eidx).*) {
                .retval_future, .noreturn => {
                    // Copy return value from callee to caller's return slot
                    // copy_to creates a new entity with properly allocated memory
                    const new_idx = try state.refinements.at(pointee_idx).*.copyTo(state.refinements, caller_refinements);
                    // Move the value to return_eidx and clear the intermediate entity
                    // This avoids double-free since only return_eidx owns the allocations
                    caller_refinements.at(return_eidx).* = caller_refinements.at(new_idx).*;
                    caller_refinements.at(new_idx).* = .{ .void = {} };
                },
                else => {
                    @panic("ret_load: multiple returns in same branch - merge not yet implemented");
                },
            }
        }

        try splat(.ret_load, state, index, self);
    }
};

/// Entity operation: MODIFY
/// Modifies the pointee entity with the stored value's state.
///
/// Store writes a value through a pointer. The store instruction itself is void.
/// Used for both `store` and `store_safe` AIR instructions (semantically identical
/// for our analysis - the difference is only runtime safety checks).
///
/// AIR Semantics:
/// - `ptr` is the instruction containing a pointer entity (from alloc, arg, etc.)
/// - `src` is the instruction containing the value to store (may be null for constants)
///
/// Entity Structure:
/// - The ptr instruction contains a pointer entity: .pointer = { .to = pointee_idx }
/// - Analyses follow the pointer to get the pointee entity
/// - The pointee's analysis fields (undefined, memory_safety) are updated
///
/// Interprocedural Pointer Args:
/// For `fn set_value(ptr: *u8) { ptr.* = 5; }`:
/// - After arg, instruction 0 contains P1' (copy of caller's pointer) -> S1' (copy of pointee)
/// - store_safe(ptr=0) follows P1' to S1' and marks S1'.undefined = defined
/// - On function close, backPropagate propagates S1's state back to caller via caller_ref
///
/// We do NOT modify the ptr instruction's refinement - it still points to the same
/// pointer entity. We only update the pointee's analysis state.
pub const Store = struct {
    /// Index into results[] for the pointer being stored through.
    /// Null when storing to a global or constant pointer (interned value).
    ptr: ?usize,
    /// Source value being stored.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        // The ptr instruction's entity is unchanged - we only update the pointee's analysis state
        try splat(.store, state, index, self);
    }
};

/// Entity operation: CREATE-SEMIDEEP
/// Creates new entity via semideep copy from error union payload.
///
/// UnwrapErrunionPayload extracts the success value from an error union (e.g., `try x` or `x catch`).
/// Uses semideep copy semantics: recursively copies values, but pointer targets are shared.
pub const UnwrapErrunionPayload = struct {
    /// Source error union being unwrapped.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.src) {
            .eidx => |src| {
                // Get the errorunion refinement and extract its payload
                const src_eidx = state.results[src].refinement orelse
                    std.debug.panic("unwrap_errunion_payload: source inst {d} has no refinement", .{src});
                const src_ref = state.refinements.at(src_eidx).*;
                // errorunion's .to points to the payload entity
                const payload_idx = src_ref.errorunion.to;
                state.results[index].refinement = payload_idx;
            },
            .interned, .other => {
                std.debug.panic("unwrap_errunion_payload: interned/other sources not supported", .{});
            },
        }
        try splat(.unwrap_errunion_payload, state, index, self);
    }
};

pub fn Simple(comptime instr: anytype) type {
    return struct {
        pub fn apply(self: @This(), state: State, index: usize) !void {
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
            try splat(instr, state, index, self);
        }
    };
}

/// Overflow operations (add_with_overflow, sub_with_overflow, etc.) return a
/// struct { result: T, overflow: u1 }. We create a two-field struct with both
/// fields as scalars. The Undefined analysis marks them as defined via splat.
pub fn OverflowOp(comptime instr: anytype) type {
    return struct {
        pub fn apply(self: @This(), state: State, index: usize) !void {
            const allocator = state.refinements.list.allocator;

            // Create two scalar fields (result and overflow flag)
            const field0_idx = try state.refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
            const field1_idx = try state.refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });

            // clobberInst takes ownership of fields slice
            const fields = allocator.alloc(EIdx, 2) catch @panic("out of memory");
            fields[0] = field0_idx;
            fields[1] = field1_idx;

            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .@"struct" = .{ .fields = fields, .type_id = 0 } });
            try splat(instr, state, index, self);
        }
    };
}

pub fn Unimplemented(comptime opts: anytype) type {
    const known_void = if (@hasField(@TypeOf(opts), "void")) opts.void else false;
    return struct {
        pub fn apply(self: @This(), state: State, index: usize) !void {
            _ = self;
            if (known_void) {
                _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
            } else {
                _ = try Inst.clobberInst(state.refinements, state.results, index, .unimplemented);
            }
        }
    };
}

/// Unreach marks the current path as unreachable (noreturn).
pub const Unreach = struct {
    pub fn apply(_: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .noreturn);
    }
};

/// IsNonNull checks if an optional is non-null.
/// Sets the optional's null_safety to .unknown with check info.
pub const IsNonNull = struct {
    /// Source optional being checked.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Result is a boolean scalar (the result of the null check)
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
        try splat(.is_non_null, state, index, self);
    }
};

/// IsNull checks if an optional is null.
/// Sets the optional's null_safety to .unknown with check info (inverted).
pub const IsNull = struct {
    /// Source optional being checked.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Result is a boolean scalar (the result of the null check)
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
        try splat(.is_null, state, index, self);
    }
};

/// Union tag check info - set when condition is a union tag comparison (get_union_tag + cmp_eq)
pub const UnionTagCheck = struct {
    union_inst: usize, // instruction index that holds the union
    field_index: u32, // the variant field being checked
};

/// CondBr is emitted at the start of each branch to trigger null_safety refinement.
/// The handler searches for optionals whose .unknown.inst matches condition_idx
/// and updates them to .non_null or .null based on the branch.
pub const CondBr = struct {
    /// true for true branch, false for false branch
    branch: bool,
    /// Instruction index that produced the condition (e.g., is_non_null).
    /// Null when condition comes from a comptime value.
    condition_idx: ?usize,
    /// If the condition is a union tag comparison, this contains the union and variant info.
    /// Used by variant_safety to know which variant is active in the true branch.
    union_tag: ?UnionTagCheck = null,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = index;
        try splat(.cond_br, state, 0, self);
    }
};

/// Noop tag for unreferenced instructions with garbage data.
/// These instructions exist in the AIR array but are not in any body.
pub const Noop = struct {
    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = self;
        _ = state;
        _ = index;
        // Do nothing - placeholder for unreferenced instructions
    }
};

/// Entity operation: MODIFY
/// Modifies the union entity to activate the specified field.
///
/// SetUnionTag sets the active tag on a union without setting the payload.
/// This marks the specified field as active (undefined) and all others as inactive.
pub const SetUnionTag = struct {
    ptr: ?usize, // union pointer instruction
    field_index: ?usize, // which field is being activated
    ty: Type, // type of the activated field

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Result is void (no new entity created)
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);

        // Follow the pointer to get the union refinement - Zig's safety checks will panic on wrong types
        const field_idx = self.field_index.?;
        const ptr_eidx = state.results[self.ptr.?].refinement.?;
        const union_eidx = state.refinements.at(ptr_eidx).pointer.to;

        // Deactivate all fields
        const fields = state.refinements.at(union_eidx).@"union".fields;
        for (fields) |*field| field.* = null;

        // Create a fresh entity with the correct type for the active field
        // NOTE: typeToRefinement and appendEntity may reallocate the refinements list,
        // so we must re-fetch the fields pointer AFTER these calls
        const field_ref = try typeToRefinement(self.ty, state.refinements);
        const new_field_eidx = try state.refinements.appendEntity(field_ref);

        // Re-fetch fields pointer after potential reallocation
        state.refinements.at(union_eidx).@"union".fields[field_idx] = new_field_eidx;

        try splat(.set_union_tag, state, index, self);
    }
};

/// GetUnionTag gets the active tag from a union.
/// Produces a scalar result (the tag value).
pub const GetUnionTag = struct {
    operand: ?usize, // the union instruction we're getting the tag from

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
        try splat(.get_union_tag, state, index, self);
    }
};

/// Entity operation: CREATE-SEMIDEEP
/// Creates new union entity via semideep copy from operand.
///
/// UnionInit initializes a union with one active field.
/// Only the specified field gets a real entity; others are null (inactive).
pub const UnionInit = struct {
    field_index: usize,
    init: Src,
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create union refinement from type info
        const union_fields = self.ty.ty.@"union";
        const allocator = state.refinements.list.allocator;

        // Union fields use ?EIdx - null means inactive, some means active
        const fields = allocator.alloc(?Refinements.EIdx, union_fields.len) catch @panic("out of memory");

        for (union_fields, 0..) |_, i| {
            if (i == self.field_index) {
                // Active field - create entity from init value
                switch (self.init) {
                    .eidx => |src| {
                        // Runtime value - share the source entity
                        fields[i] = state.results[src].refinement;
                    },
                    .interned => |init_ty| {
                        // Comptime value - create entity from type
                        const field_ref = try typeToRefinement(init_ty, state.refinements);
                        fields[i] = try state.refinements.appendEntity(field_ref);
                    },
                    .other => @panic("UnionInit: .other source not yet implemented"),
                }
            } else {
                // Inactive field
                fields[i] = null;
            }
        }

        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .@"union" = .{ .fields = fields, .type_id = self.ty.id orelse 0 } });
        try splat(.union_init, state, index, self);
    }
};

pub const AnyTag = union(enum) {
    // Noop for unreferenced instructions
    noop: Noop,

    // Implemented tags
    alloc: Alloc,
    alloc_create: AllocCreate,
    alloc_destroy: AllocDestroy,
    arg: Arg,
    bitcast: Bitcast,
    br: Br,
    dbg_stmt: DbgStmt,
    dbg_var_ptr: DbgVarPtr, // Names a pointer variable (for stack pointer tracking)
    dbg_var_val: DbgVarVal, // Names a value variable (no pointer tracking needed)
    dbg_arg_inline: DbgVarPtr, // Same structure as dbg_var_ptr
    load: Load,
    optional_payload: OptionalPayload,
    field_parent_ptr: FieldParentPtr,
    ret_safe: RetSafe,
    store: Store,
    store_safe: Store, // Same as store, just with runtime safety checks
    unwrap_errunion_payload: UnwrapErrunionPayload,

    // Simple tags - math/comparison operations that produce scalar values
    // this will have to be refined to pass parameters and divide into BinOp and UnOp.
    bit_and: Simple(.bit_and),
    cmp_eq: Simple(.cmp_eq),
    cmp_gt: Simple(.cmp_gt),
    cmp_lte: Simple(.cmp_lte),
    ctz: Simple(.ctz),
    sub: Simple(.sub),

    // Unimplemented tags (no-op)
    add_with_overflow: OverflowOp(.add_with_overflow),
    aggregate_init: Unimplemented(.{}),
    array_to_slice: Unimplemented(.{}),
    block: Block,
    cond_br: CondBr,
    dbg_inline_block: Unimplemented(.{}),
    intcast: Unimplemented(.{}),
    is_non_err: Simple(.is_non_err), // produces boolean scalar
    is_non_null: IsNonNull,
    is_null: IsNull,
    memset_safe: Unimplemented(.{ .void = true }),
    noop_pruned_debug: Unimplemented(.{ .void = true }),
    ptr_add: Unimplemented(.{}),
    ret_addr: Unimplemented(.{}),
    ret_load: RetLoad,
    ret_ptr: RetPtr,
    slice: Unimplemented(.{}),
    slice_len: Unimplemented(.{}),
    stack_trace_frames: Unimplemented(.{}),
    struct_field_ptr: StructFieldPtr,
    struct_field_val: StructFieldVal,
    sub_with_overflow: OverflowOp(.sub_with_overflow),
    @"try": UnwrapErrunionPayload, // try extracts payload from error union, same as unwrap_errunion_payload
    unreach: Unreach,
    unwrap_errunion_err: Simple(.unwrap_errunion_err), // produces error scalar
    wrap_errunion_err: Unimplemented(.{}),
    wrap_errunion_payload: Unimplemented(.{}),

    // Union tags - variant safety not implemented yet
    set_union_tag: SetUnionTag,
    get_union_tag: GetUnionTag,
    union_init: UnionInit,
};

pub fn splat(comptime tag: anytype, state: State, index: usize, payload: anytype) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, @tagName(tag))) {
            try @field(Analysis, @tagName(tag))(state, index, payload);
        }
    }
}

/// Called at the end of each function to allow analyses to perform final checks.
/// Each analysis can implement `onFinish` to do end-of-function processing
/// (e.g., memory leak detection after all paths have been processed).
pub fn splatFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "onFinish")) {
            try Analysis.onFinish(results, ctx, refinements);
        }
    }
}

/// Called for each orphaned entity detected during branch merge.
/// An orphaned entity is one that was created during branch execution but
/// is no longer reachable from any result slot after the branch completes.
/// Analysis modules can implement `orphaned` to handle these (e.g., detect leaked allocations).
fn splatOrphaned(ctx: *Context, refinements: *Refinements, branch_refinements: *Refinements, eidx: EIdx) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "orphaned")) {
            try Analysis.orphaned(ctx, refinements, branch_refinements, eidx);
        }
    }
}

/// Check if a branch contains any .noreturn result, making it unreachable.
fn branchIsUnreachable(branch_results: []const Inst, branch_refinements: *Refinements) bool {
    for (branch_results) |result| {
        const eidx = result.refinement orelse continue;
        if (branch_refinements.at(eidx).* == .noreturn) {
            return true;
        }
    }
    return false;
}

/// Merge branch results after a conditional or switch.
/// Walks through all result slots and calls each analysis's merge function.
/// Analysis modules can implement `merge` to handle their specific fields.
///
/// Multiple result slots may reference the same EIdx internally. We track
/// which EIdx values have been merged to avoid re-merging the same entity.
pub fn splatMerge(
    comptime merge_tag: anytype,
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    branches: []const State,
) !void {
    const allocator = ctx.allocator;

    // Record base_len before merge - entities >= this index were created by branches
    const base_len = refinements.list.items.len;

    // Build filtered branch array - null for unreachable
    const filtered = try allocator.alloc(?State, branches.len);
    defer allocator.free(filtered);

    for (branches, 0..) |branch, i| {
        filtered[i] = if (branchIsUnreachable(branch.results, branch.refinements))
            null
        else
            branch;
    }

    // Track merged EIdx to avoid re-merging the same entity
    var merged = std.AutoHashMap(EIdx, void).init(allocator);
    defer merged.deinit();

    // Reusable array for branch EIdx values
    const branch_eidxs = try allocator.alloc(?EIdx, filtered.len);
    defer allocator.free(branch_eidxs);

    // Walk through all result slots
    for (results, 0..) |*result, result_idx| {
        const orig_eidx = result.refinement orelse continue;

        // Build branch EIdx array for this result slot
        for (filtered, 0..) |branch_opt, i| {
            branch_eidxs[i] = if (branch_opt) |branch| branch.results[result_idx].refinement else null;
        }

        // Recursively merge this refinement and all nested refinements
        try mergeRefinementRecursive(
            merge_tag,
            allocator,
            ctx,
            refinements,
            orig_eidx,
            filtered,
            branch_eidxs,
            &merged,
        );
    }

    // Detect orphaned entities: created by branch but not reachable after merge
    for (filtered) |branch_opt| {
        const branch = branch_opt orelse continue;
        const branch_len = branch.refinements.list.items.len;

        // Check entities created by this branch (indices >= base_len)
        // Skip if branch has no new entities (branch_len <= base_len)
        if (branch_len <= base_len) continue;

        for (base_len..branch_len) |eidx_usize| {
            const eidx: EIdx = @intCast(eidx_usize);
            // If not in merged set, it's orphaned (unreachable from results)
            if (!merged.contains(eidx)) {
                try splatOrphaned(ctx, refinements, branch.refinements, eidx);
            }
        }
    }
}

/// Follow .to field for each branch's eidx, updating branch_eidxs in place.
fn followBranchEidxs(
    comptime tag: std.meta.Tag(Refinement),
    branches: []const ?State,
    branch_eidxs: []?EIdx,
) void {
    for (branches, branch_eidxs) |branch_opt, *eidx| {
        const branch = branch_opt orelse continue;
        eidx.* = @field(branch.refinements.at(eidx.*.?).*, @tagName(tag)).to;
    }
}

/// Recursively merge a refinement and its nested refinements.
/// Calls analysis merge functions at each node, then recurses into children.
fn mergeRefinementRecursive(
    comptime merge_tag: anytype,
    allocator: std.mem.Allocator,
    ctx: *Context,
    refinements: *Refinements,
    orig_eidx: EIdx,
    branches: []const ?State,
    branch_eidxs: []?EIdx,
    merged: *std.AutoHashMap(EIdx, void),
) !void {
    // Skip if we've already merged this entity
    if (merged.contains(orig_eidx)) return;
    try merged.put(orig_eidx, {});

    // Call each analyzer's merge for this node
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "merge")) {
            try Analysis.merge(
                ctx,
                merge_tag,
                refinements,
                orig_eidx,
                branches,
                branch_eidxs,
            );
        }
    }

    // Recurse into children based on refinement type
    const orig_ref = refinements.at(orig_eidx);
    switch (orig_ref.*) {
        // Pointer: check rejection logic before following and merging
        .pointer => |p| {
            // TODO: pointer rejection logic (all same original OR all new, not mixed)
            followBranchEidxs(.pointer, branches, branch_eidxs);
            try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, p.to, branches, branch_eidxs, merged);
        },
        // For optional/errorunion: follow .to in all branches
        // Types always match when following the same structural path
        // If branch exists, eidx exists (they're always in sync)
        inline .optional, .errorunion => |data, tag| {
            followBranchEidxs(tag, branches, branch_eidxs);
            try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, data.to, branches, branch_eidxs, merged);
        },
        .@"struct" => |s| {
            // Need separate array for struct fields since we recurse multiple times
            const field_eidxs = try allocator.alloc(?EIdx, branches.len);
            defer allocator.free(field_eidxs);
            for (s.fields, 0..) |field_idx, field_i| {
                for (branches, branch_eidxs, 0..) |branch_opt, branch_eidx_opt, i| {
                    const branch = branch_opt orelse {
                        field_eidxs[i] = null;
                        continue;
                    };
                    // Types always match; if branch exists, eidx exists
                    field_eidxs[i] = branch.refinements.at(branch_eidx_opt.?).@"struct".fields[field_i];
                }
                try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, field_idx, branches, field_eidxs, merged);
            }
        },
        .@"union" => |u| {
            // Need separate array for union fields since we recurse multiple times
            const field_eidxs = try allocator.alloc(?EIdx, branches.len);
            defer allocator.free(field_eidxs);
            for (u.fields, 0..) |field_opt, field_i| {
                const field_idx = field_opt orelse continue;
                for (branches, branch_eidxs, 0..) |branch_opt, branch_eidx_opt, i| {
                    const branch = branch_opt orelse {
                        field_eidxs[i] = null;
                        continue;
                    };
                    // Types always match; if branch exists, eidx exists
                    field_eidxs[i] = branch.refinements.at(branch_eidx_opt.?).@"union".fields[field_i];
                }
                try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, field_idx, branches, field_eidxs, merged);
            }
        },
        else => {},
    }
}

// Tests

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_eidx = 0,
        .caller_refinements = null,
    };
}

/// Test getName function that maps name IDs to strings for tests
fn testGetName(id: u32) []const u8 {
    return switch (id) {
        1 => "my_var",
        2 => "my_val",
        3 => "param",
        4 => "foo",
        else => "unknown",
    };
}

test "dbg_var_ptr sets name on target instruction" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.getName = &testGetName;
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc to create a pointer
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try std.testing.expect(results[1].name_id == null);

    // dbg_var_ptr should set the name_id on the target instruction
    try Inst.apply(state, 2, .{ .dbg_var_ptr = .{ .ptr = 1, .name_id = 1 } });

    try std.testing.expectEqual(@as(u32, 1), results[1].name_id.?);
}

test "dbg_var_ptr with null ptr does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.getName = &testGetName;
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // dbg_var_ptr with null ptr should not crash
    try Inst.apply(state, 1, .{ .dbg_var_ptr = .{ .ptr = null, .name_id = 1 } });
}

test "dbg_var_val sets name on target instruction" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.getName = &testGetName;
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Set up a result at index 1
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    try std.testing.expect(results[1].name_id == null);

    // dbg_var_val should set the name_id on the target instruction
    try Inst.apply(state, 2, .{ .dbg_var_val = .{ .ptr = 1, .name_id = 2 } });

    try std.testing.expectEqual(@as(u32, 2), results[1].name_id.?);
}

test "arg copies value and sets name" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &testGetName;

    // Caller refinements (simulating the caller)
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const arg_eidx = try caller_refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });

    // Callee refinements
    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // arg should copy the entity from caller and store its tag
    try Inst.apply(state, 0, .{ .arg = .{ .value = .{ .eidx = arg_eidx }, .name_id = 3 } });

    try std.testing.expect(results[0].refinement != null);
    // name_id is now in the inst_tag, not a separate field
    try std.testing.expectEqual(@as(u32, 3), results[0].inst_tag.?.arg.name_id);
}

test "br shares source refinement with block" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create a block at index 0
    try Inst.apply(state, 0, .{ .block = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // Create a value at index 1
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const src_eidx = results[1].refinement.?;

    // br should share the source refinement with the block
    try Inst.apply(state, 2, .{ .br = .{ .block = 0, .src = .{ .eidx = 1 } } });

    // Block should now have the same refinement as source
    try std.testing.expectEqual(src_eidx, results[0].refinement.?);
}

test "dbg_stmt does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    // dbg_stmt should not crash and should set void refinement
    try Inst.apply(state, 0, .{ .dbg_stmt = .{ .line = 10, .column = 5 } });

    try std.testing.expect(results[0].refinement != null);
    try std.testing.expectEqual(.void, std.meta.activeTag(refinements.at(results[0].refinement.?).*));
}

test "optional_payload extracts inner value from optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create an optional with a scalar inside, marked as non-null (checked)
    const inner_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .non_null = .{
            .function = "test",
            .file = "test.zig",
            .line = 1,
        } } },
        .type_id = 0,
        .to = inner_eidx,
    } });
    results[0].refinement = opt_eidx;

    // optional_payload should extract the inner value
    try Inst.apply(state, 1, .{ .optional_payload = .{ .src = .{ .eidx = 0 } } });

    // Result should point to the inner scalar
    try std.testing.expectEqual(inner_eidx, results[1].refinement.?);
}

test "unwrap_errunion_payload extracts payload from error union" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create an error union with a scalar payload
    const payload_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const eu_eidx = try refinements.appendEntity(.{ .errorunion = .{ .analyte = .{}, .type_id = 0, .to = payload_eidx } });
    results[0].refinement = eu_eidx;

    // unwrap_errunion_payload should extract the payload
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

    // Result should point to the payload
    try std.testing.expectEqual(payload_eidx, results[1].refinement.?);
}

test "Simple tags produce scalar" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 6;
    const state = testState(&ctx, &results, &refinements);

    // All Simple tags should produce scalar refinements
    try Inst.apply(state, 0, .{ .bit_and = .{} });
    try Inst.apply(state, 1, .{ .cmp_eq = .{} });
    try Inst.apply(state, 2, .{ .cmp_gt = .{} });
    try Inst.apply(state, 3, .{ .cmp_lte = .{} });
    try Inst.apply(state, 4, .{ .ctz = .{} });
    try Inst.apply(state, 5, .{ .sub = .{} });

    for (0..6) |i| {
        try std.testing.expect(results[i].refinement != null);
        try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(results[i].refinement.?).*));
    }
}

test "block creates refinement based on type" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Block with scalar type
    try Inst.apply(state, 0, .{ .block = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try std.testing.expect(results[0].refinement != null);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(results[0].refinement.?).*));

    // Block with void type
    try Inst.apply(state, 1, .{ .block = .{ .ty = .{ .id = null, .ty = .{ .void = {} } } } });
    try std.testing.expect(results[1].refinement != null);
    try std.testing.expectEqual(.void, std.meta.activeTag(refinements.at(results[1].refinement.?).*));
}

test "unreach does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 1;
    const state = testState(&ctx, &results, &refinements);

    // unreach should set noreturn refinement
    try Inst.apply(state, 0, .{ .unreach = .{} });

    try std.testing.expect(results[0].refinement != null);
    try std.testing.expectEqual(.noreturn, std.meta.activeTag(refinements.at(results[0].refinement.?).*));
}

test "struct_field_ptr gets pointer to struct field" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create a pointer to a struct with two scalar fields
    const field0_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const field1_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const fields = try allocator.alloc(EIdx, 2);
    fields[0] = field0_eidx;
    fields[1] = field1_eidx;
    const struct_eidx = try refinements.appendEntity(.{ .@"struct" = .{
        .analyte = .{},
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = struct_eidx } });
    results[0].refinement = ptr_eidx;

    // struct_field_ptr should return pointer to field 1
    try Inst.apply(state, 1, .{ .struct_field_ptr = .{ .base = 0, .field_index = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    try std.testing.expect(results[1].refinement != null);
    const result_ref = refinements.at(results[1].refinement.?);
    try std.testing.expectEqual(.pointer, std.meta.activeTag(result_ref.*));
    // The pointer should point to field 1
    try std.testing.expectEqual(field1_eidx, result_ref.pointer.to);
}

test "struct_field_val extracts field value from struct" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create a struct with two scalar fields (both defined)
    const field0_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = .{ .defined = {} } }, .type_id = 0 } });
    const field1_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = .{ .defined = {} } }, .type_id = 0 } });
    const fields = try allocator.alloc(EIdx, 2);
    fields[0] = field0_eidx;
    fields[1] = field1_eidx;
    const struct_eidx = try refinements.appendEntity(.{ .@"struct" = .{
        .analyte = .{},
        .fields = fields,
        .type_id = 0,
    } });
    results[0].refinement = struct_eidx;

    // struct_field_val should extract field 0
    try Inst.apply(state, 1, .{ .struct_field_val = .{ .operand = 0, .field_index = 0, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // Result should share the field's entity
    try std.testing.expectEqual(field0_eidx, results[1].refinement.?);
}

test "is_non_null records check on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create an optional
    const inner_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .analyte = .{}, .type_id = 0, .to = inner_eidx } });
    results[0].refinement = opt_eidx;

    // is_non_null should record the check
    try Inst.apply(state, 1, .{ .is_non_null = .{ .src = .{ .eidx = 0 } } });

    // The optional's null_safety should be set to unknown with check info
    const opt_ref = refinements.at(opt_eidx);
    try std.testing.expect(opt_ref.optional.analyte.null_safety != null);
    try std.testing.expectEqual(.unknown, std.meta.activeTag(opt_ref.optional.analyte.null_safety.?));
}

test "is_null records check on optional" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create an optional
    const inner_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const opt_eidx = try refinements.appendEntity(.{ .optional = .{ .analyte = .{}, .type_id = 0, .to = inner_eidx } });
    results[0].refinement = opt_eidx;

    // is_null should record the check
    try Inst.apply(state, 1, .{ .is_null = .{ .src = .{ .eidx = 0 } } });

    // The optional's null_safety should be set to unknown with check info
    const opt_ref = refinements.at(opt_eidx);
    try std.testing.expect(opt_ref.optional.analyte.null_safety != null);
    try std.testing.expectEqual(.unknown, std.meta.activeTag(opt_ref.optional.analyte.null_safety.?));
}

test "set_union_tag updates union variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create a union with two fields
    const field0_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const field1_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const fields = try allocator.alloc(?EIdx, 2);
    fields[0] = field0_eidx;
    fields[1] = field1_eidx;
    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{},
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = union_eidx } });
    results[0].refinement = ptr_eidx;

    // set_union_tag should update the active variant
    try Inst.apply(state, 1, .{ .set_union_tag = .{ .ptr = 0, .field_index = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // The union's active field should be set (field 1), others null
    const union_ref = refinements.at(union_eidx);
    try std.testing.expect(union_ref.@"union".fields[0] == null);
    try std.testing.expect(union_ref.@"union".fields[1] != null);
}

test "get_union_tag produces scalar" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create a union
    const field_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    const fields = try allocator.alloc(?EIdx, 1);
    fields[0] = field_eidx;
    const union_eidx = try refinements.appendEntity(.{ .@"union" = .{
        .analyte = .{},
        .fields = fields,
        .type_id = 0,
    } });
    results[0].refinement = union_eidx;

    // get_union_tag should produce a scalar (the tag value)
    try Inst.apply(state, 1, .{ .get_union_tag = .{ .operand = 0 } });

    try std.testing.expect(results[1].refinement != null);
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(results[1].refinement.?).*));
}

test "union_init creates union with active variant" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create a value to use as the init value
    const val_eidx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    results[0].refinement = val_eidx;

    // union_init should create a union with active variant set
    try Inst.apply(state, 1, .{ .union_init = .{
        .ty = .{ .id = null, .ty = .{ .@"union" = &.{ .{ .id = null, .ty = .{ .scalar = {} } }, .{ .id = null, .ty = .{ .scalar = {} } } } } },
        .field_index = 1,
        .init = .{ .eidx = 0 },
    } });

    try std.testing.expect(results[1].refinement != null);
    const union_ref = refinements.at(results[1].refinement.?);
    try std.testing.expectEqual(.@"union", std.meta.activeTag(union_ref.*));
    // Field 1 should be active (non-null), field 0 inactive (null)
    try std.testing.expect(union_ref.@"union".fields[0] == null);
    try std.testing.expect(union_ref.@"union".fields[1] != null);
}
