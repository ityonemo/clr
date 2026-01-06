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
const Gid = Refinements.Gid;
const Context = @import("Context.zig");
const State = Inst.State;
const UndefinedSafety = @import("analysis/undefined_safety.zig").UndefinedSafety;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
const NullSafety = @import("analysis/null_safety.zig").NullSafety;
const VariantSafety = @import("analysis/variant_safety.zig").VariantSafety;
const FieldParentPtrSafety = @import("analysis/fieldparentptr_safety.zig").FieldParentPtrSafety;
pub const analyses = .{ UndefinedSafety, MemorySafety, NullSafety, VariantSafety, FieldParentPtrSafety };

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
        allocator: Name, // allocator type identified by type_id (vtable FQN hash)
        void: void,
    }
};

/// Source reference for instructions - indicates where a value comes from.
/// Used by store, br, ret_safe, and other tags that reference source values,
/// as well as by calls.
pub const Src = union(enum) {
    /// Runtime value from a result in the results table (index into results[])
    inst: usize,
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
        .scalar => .{ .scalar = .{ .type_id = type_id } },
        .void => .void,
        .pointer => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .pointer = .{ .type_id = type_id, .to = child_idx } };
        },
        .optional => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .type_id = type_id, .to = child_idx } };
        },
        .errorunion => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .errorunion = .{ .type_id = type_id, .to = child_idx } };
        },
        .null => |child| {
            // .null is a null optional value - creates same structure as .optional
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .type_id = type_id, .to = child_idx } };
        },
        .undefined => |child| {
            // .undefined wraps a type - recurse into inner type.
            // The undefined.store handler checks for .undefined wrapper and marks as undefined.
            return typeToRefinement(child.*, refinements);
        },
        .allocator => |alloc_type_id| {
            // Allocator refinement - type_id uniquely identifies allocator type
            return .{ .allocator = .{ .type_id = alloc_type_id } };
        },
        .region => |child| {
            // Region (array/slice) - create uniform element entity
            // All elements share this single refinement (uniform region model)
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            // Region container has null undefined (only child has undefined state)
            return .{ .region = .{ .type_id = type_id, .to = child_idx } };
        },
        inline .@"struct", .@"union" => |type_fields, class_tag| blk: {
            const allocator = refinements.list.allocator;
            const FieldType = if (class_tag == .@"union") ?Gid else Gid;
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
        try splat(.alloc, state, index, self);
    }
};

pub const AllocCreate = struct {
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages
    allocator_inst: ?usize, // Optional instruction index for runtime allocator (to read type_id from refinement)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // AllocCreate returns Allocator.Error!*T, so we create errorunion -> ptr -> T
        // Create pointee from type info
        const pointee_ref = try typeToRefinement(self.ty, state.refinements);
        const pointee_idx = try state.refinements.appendEntity(pointee_ref);
        // Create pointer entity pointing to the typed pointee
        const ptr_idx = try state.refinements.appendEntity(.{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
        // Wrap in error union
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .errorunion = .{ .type_id = 0, .to = ptr_idx } });
        try splat(.alloc_create, state, index, self);
    }
};

pub const AllocDestroy = struct {
    /// Index into results[] array for the pointer being freed
    ptr: usize,
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages
    allocator_inst: ?usize, // Optional instruction index for runtime allocator (to read type_id from refinement)

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        try splat(.alloc_destroy, state, index, self);
    }
};

/// Entity operation: CREATE
/// Creates an .allocator refinement from a call to .allocator() method.
/// The type_id uniquely identifies the allocator type (e.g., GPA vs PageAllocator).
pub const MkAllocator = struct {
    type_id: u32, // Allocator type ID (FQN hash of .allocator() method)

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create an allocator refinement with the type_id
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{
            .allocator = .{ .type_id = self.type_id },
        });
        try splat(.mkallocator, state, index, self);
    }
};

/// Entity operation: SHARE (global refinements) or CREATE (interned)
/// For runtime args: shares the caller's entity directly via global GID.
/// For compile-time args: creates entity in global refinements.
///
/// AIR Semantics:
/// - AIR arg instructions contain VALUES directly, not pointers to stack locations.
/// - If the parameter type is `*u8`, the instruction contains the pointer value.
/// - If the parameter type is `u8`, the instruction contains the scalar value.
/// - Taking `&param` in source code generates explicit `alloc` + `store_safe` in AIR.
///
/// Global Refinements Architecture:
/// - With a single global refinements table, the caller's GID IS the callee's GID.
/// - No copying needed - callee directly references caller's entity.
/// - No backpropagation needed - modifications are direct to the shared entity.
///
/// Example for `fn set_value(ptr: *u8) { ptr.* = 5; }`:
/// - Caller passes pointer entity P1 -> scalar S1 (undefined) at some GID
/// - Arg stores that same GID in results[index].refinement
/// - store_safe(ptr=0) follows P1 to S1, marks S1 as defined (direct modification)
pub const Arg = struct {
    name_id: u32, // Parameter name ID, resolved via ctx.getName()
    /// Source of the argument value - either runtime (.gid) or compile-time (.interned)
    value: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.value) {
            .inst => |src_gid| {
                // Global refinements: caller's GID is the same table, use directly
                state.results[index].refinement = @intCast(src_gid);
                // name_id is in inst_tag (stored by Inst.apply)
            },
            .interned => |ty| {
                // Compile-time constant - create entity from type info in global table
                const ref = try typeToRefinement(ty, state.refinements);
                const local_idx = try state.refinements.appendEntity(ref);
                state.results[index].refinement = local_idx;
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
            .inst => |src| {
                const src_gid = state.results[src].refinement orelse return;
                const src_ref = state.refinements.at(src_gid);

                // Check if we're converting pointer to optional (e.g., *T to ?*T)
                if (self.ty.ty == .optional and src_ref.* == .pointer) {
                    // Create optional wrapper that points to the existing pointer entity
                    const opt_gid = try state.refinements.appendEntity(.{
                        .optional = .{ .type_id = 0, .to = src_gid },
                    });
                    state.results[index].refinement = opt_gid;
                } else {
                    // Default: share the source refinement directly
                    state.results[index].refinement = src_gid;
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
/// - `break :blk value` → src is .gid or .interned with the value
/// - `break :blk` (void) → src is .interned with .void type
pub const Br = struct {
    block: usize,
    /// Value being passed to the block.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // For gid sources, share the source's refinement with the block.
        // This ensures operations on the block (like alloc_destroy) affect the same entity.
        // Clone preserves indices, so sharing propagates correctly through merge.
        switch (self.src) {
            .inst => |src| {
                const src_gid = state.results[src].refinement orelse return;
                state.results[self.block].refinement = src_gid;
            },
            // Interned values are handled by analysis modules (e.g., undefined.br marks as defined)
            .interned, .other => {},
        }
        try splat(.br, state, index, self);
    }

    /// Copy analysis state (memory_safety, undefined) from source entity to destination.
    /// This preserves the destination's type structure while updating its analysis state.
    fn copyAnalyteState(refns: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const dst = refns.at(dst_gid);
        const src = refns.at(src_gid);

        switch (dst.*) {
            .pointer => |*dp| {
                if (src.* == .pointer) {
                    const sp = src.pointer;
                    // Copy analyte (memory_safety, undefined)
                    dp.analyte = sp.analyte;
                    // Recursively copy pointee's analysis state
                    copyAnalyteState(refns, dp.to, sp.to);
                }
            },
            .scalar => |*ds| {
                if (src.* == .scalar) {
                    ds.* = src.scalar;
                }
            },
            .optional => |*d_opt| {
                if (src.* == .optional) {
                    copyAnalyteState(refns, d_opt.to, src.optional.to);
                }
            },
            .@"struct" => |*d_struct| {
                if (src.* == .@"struct") {
                    const s_struct = src.@"struct";
                    for (d_struct.fields, s_struct.fields) |df, sf| {
                        copyAnalyteState(refns, df, sf);
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
            .@"struct" => |data| {
                const field_idx = data.fields[self.field_index];
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = field_idx } });
            },
            .@"union" => |data| {
                const field_idx = idx: {
                    if (data.fields[self.field_index]) |idx| break :idx idx;
                    // Field is inactive - check if this is a tagged union
                    if (data.analyte.variant_safety != null) {
                        // Tagged union: accessing inactive field is an error
                        try state.ctx.meta.print(state.ctx.writer, "access of inactive union variant in ", .{});
                        return error.InactiveVariantAccess;
                    }
                    // Untagged union: create entity on first access and persist it
                    const new_field_ref = try typeToRefinement(self.ty.ty.pointer.*, state.refinements);
                    const new_idx = try state.refinements.appendEntity(new_field_ref);
                    state.refinements.at(container_idx).@"union".fields[self.field_index] = new_idx;
                    break :idx new_idx;
                };
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = field_idx } });
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
            inline .@"struct", .@"union" => |data, container_tag| {
                // Get field index - for unions, create entity for inactive fields
                // (variant_safety analysis will report error via splat)
                const field_idx = idx: {
                    if (container_tag == .@"union") {
                        if (data.fields[self.field_index]) |idx| {
                            break :idx idx;
                        }
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
            .inst => |src| {
                const src_ref_gid = state.results[src].refinement orelse return;
                const src_ref = state.refinements.at(src_ref_gid);

                if (src_ref.* == .optional) {
                    // Extract the payload from the optional (optional.to is the inner value)
                    state.results[index].refinement = src_ref.optional.to;
                } else {
                    // Not an optional - might be error union or direct pass-through
                    state.results[index].refinement = src_ref_gid;
                }
            },
            .interned => {
                // optional_payload shouldn't receive interned values
                @panic("optional_payload: unexpected interned source");
            },
            .other => {
                // Global/external source - mark as unimplemented
                _ = try Inst.clobberInst(state.refinements, state.results, index, .unimplemented);
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
/// Global Refinements Architecture:
/// - return_gid points to a slot in the refinements table
/// - Callee writes directly to that slot
/// - No copying between tables needed
pub const RetSafe = struct {
    /// Value being returned. Use .interned with .void for void returns.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const allocator = state.ctx.allocator;

        // Return instruction produces void (not a usable value)
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);

        // Mark branch as returning (for merge exclusion)
        if (state.branch_returns) |br| {
            if (br.*) @panic("ret_safe: multiple returns in same branch");
            br.* = true;
        }

        // Clone the current refinements table for early_returns
        const cloned = try allocator.create(Refinements);
        cloned.* = try state.refinements.clone(allocator);

        // Set the return value in the CLONED table (not the original)
        const return_gid = state.return_gid;
        switch (self.src) {
            .inst => |src| {
                const src_gid = state.results[src].refinement orelse @panic("return function requested uninitialized instruction value");
                // Deep copy to avoid double-free when struct/union fields are freed
                cloned.at(return_gid).* = try cloned.deepCopyValue(cloned.at(src_gid).*);
            },
            .interned => |ty| {
                // Comptime return value
                if (ty.ty == .void) {
                    cloned.at(return_gid).* = .void;
                } else if (ty.ty == .undefined) {
                    // Explicitly undefined value - mark as undefined
                    cloned.at(return_gid).* = try typeToRefinement(ty, cloned);
                    splatInit(cloned, return_gid, state.ctx);
                } else {
                    // Non-void, non-undefined comptime value (constants, null) - mark as defined
                    cloned.at(return_gid).* = try typeToRefinement(ty, cloned);
                    splatInitDefined(cloned, return_gid, state.ctx);
                }
            },
            .other => {
                // Global/other source - not yet supported
                @panic("ret_safe: .other source not yet implemented");
            },
        }

        // Append cloned state to early_returns (merging happens at function end)
        if (state.early_returns) |early_returns| {
            try early_returns.append(allocator, State{
                .ctx = state.ctx,
                .results = state.results,
                .refinements = cloned,
                .return_gid = state.return_gid,
            });
        } else {
            // No early_returns tracking - fall back to writing directly to return slot
            state.refinements.at(return_gid).* = cloned.at(return_gid).*;
            cloned.deinit();
            allocator.destroy(cloned);
        }

        // Splat runs last - analyses see state after copy is complete
        try splat(.ret_safe, state, index, self);
    }
};

/// RetPtr returns a pointer to the return value storage.
/// Used for large return values (structs, unions) where caller provides storage.
///
/// Global Refinements Architecture:
/// - return_gid points to a slot in the refinements table
/// - ret_ptr creates a pointer to a local entity for building up the return value
/// - ret_load will copy the built-up value to return_gid
pub const RetPtr = struct {
    /// The return value type (pointee type, not pointer type)
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create a local entity for the return value based on type
        const return_ref = try typeToRefinement(self.ty, state.refinements);
        const return_idx = try state.refinements.appendEntity(return_ref);

        // Create a pointer to this entity as the result
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = return_idx } });

        try splat(.ret_ptr, state, index, self);
    }
};

/// RetLoad loads from ret_ptr to complete the return.
/// Copies the return value entity to the return slot.
///
/// Global Refinements Architecture:
/// - return_gid points to a slot in the refinements table
/// - Just copy the value from ret_ptr's target to return_gid
pub const RetLoad = struct {
    /// The ret_ptr instruction index
    ptr: usize,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const allocator = state.ctx.allocator;

        // Return instruction produces void (not a usable value)
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);

        // Mark branch as returning (for merge exclusion)
        if (state.branch_returns) |br| {
            if (br.*) @panic("ret_load: multiple returns in same branch");
            br.* = true;
        }

        // Get the entity from ret_ptr's pointer
        const ptr_ref = state.results[self.ptr].refinement orelse @panic("ret_load: ret_ptr has no refinement");
        const pointee_idx = state.refinements.at(ptr_ref).pointer.to;

        // Clone the current refinements table for early_returns
        const cloned = try allocator.create(Refinements);
        cloned.* = try state.refinements.clone(allocator);

        // Set return value in the CLONED table (deep copy of pointee)
        const return_gid = state.return_gid;
        cloned.at(return_gid).* = try cloned.deepCopyValue(cloned.at(pointee_idx).*);

        // Append cloned state to early_returns (merging happens at function end)
        if (state.early_returns) |early_returns| {
            try early_returns.append(allocator, State{
                .ctx = state.ctx,
                .results = state.results,
                .refinements = cloned,
                .return_gid = state.return_gid,
            });
        } else {
            // No early_returns tracking - fall back to writing directly to return slot
            state.refinements.at(return_gid).* = cloned.at(return_gid).*;
            cloned.deinit();
            allocator.destroy(cloned);
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
            .inst => |src| {
                // Get the errorunion refinement and extract its payload
                const src_gid = state.results[src].refinement orelse
                    std.debug.panic("unwrap_errunion_payload: source inst {d} has no refinement", .{src});
                const src_ref = state.refinements.at(src_gid).*;
                // errorunion's .to points to the payload entity
                const payload_gid = src_ref.errorunion.to;
                state.results[index].refinement = payload_gid;
            },
            .interned, .other => {
                std.debug.panic("unwrap_errunion_payload: interned/other sources not supported", .{});
            },
        }
        try splat(.unwrap_errunion_payload, state, index, self);
    }
};

/// Entity operation: CREATE
/// Creates an error union containing a payload value (success case).
///
/// WrapErrunionPayload takes a payload value and wraps it in an error union.
/// This is the inverse of UnwrapErrunionPayload.
pub const WrapErrunionPayload = struct {
    /// Source value to wrap as the payload.
    src: Src,
    /// Result type (the error union type).
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create errorunion structure based on the result type
        // payload_gid is the GID of the payload entity
        const payload_gid: Gid = switch (self.src) {
            .inst => |src| blk: {
                // Get payload refinement from source instruction
                const src_gid = state.results[src].refinement orelse {
                    // Source has no refinement - create based on type
                    const ref = try typeToRefinement(self.ty.ty.errorunion.*, state.refinements);
                    break :blk try state.refinements.appendEntity(ref);
                };
                // Deep copy the payload so we have our own entity
                const src_ref = state.refinements.at(src_gid).*;
                break :blk try Refinement.copyTo(src_ref, state.refinements, state.refinements);
            },
            .interned => |i| blk: {
                // Create from type - wrap in Type struct
                const ref = try typeToRefinement(.{ .id = i.id, .ty = i.ty }, state.refinements);
                break :blk try state.refinements.appendEntity(ref);
            },
            .other => blk: {
                const ref = try typeToRefinement(self.ty.ty.errorunion.*, state.refinements);
                break :blk try state.refinements.appendEntity(ref);
            },
        };

        // Create the errorunion that points to the payload
        const errunion_gid = try state.refinements.appendEntity(.{ .errorunion = .{
            .type_id = self.ty.id orelse 0,
            .to = payload_gid,
        } });

        state.results[index].refinement = errunion_gid;
        try splat(.wrap_errunion_payload, state, index, self);
    }
};

/// Entity operation: CREATE-PTR
/// Creates pointer to error union payload.
///
/// ErrunionPayloadPtrSet takes a pointer to an error union *(E!T) and returns
/// a pointer to the payload *T. This is used when initializing error unions.
pub const ErrunionPayloadPtrSet = struct {
    /// Index into results[] for the pointer to error union.
    ptr: ?usize,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const ptr_inst = self.ptr orelse {
            // No pointer - create unimplemented
            _ = try Inst.clobberInst(state.refinements, state.results, index, .unimplemented);
            return;
        };

        // Get the pointer to error union
        const ptr_gid = state.results[ptr_inst].refinement orelse
            std.debug.panic("errunion_payload_ptr_set: ptr inst {d} has no refinement", .{ptr_inst});

        // Follow pointer to get error union
        const ptr_ref = state.refinements.at(ptr_gid).*;
        const errunion_gid = ptr_ref.pointer.to;
        const errunion_ref = state.refinements.at(errunion_gid).*;

        // Get payload GID from error union
        const payload_gid = errunion_ref.errorunion.to;

        // Create a new pointer that points to the payload
        // Pointer is defined (we have a valid address to the payload)
        const new_ptr_gid = try state.refinements.appendEntity(.{ .pointer = .{
            .analyte = .{ .undefined = .{ .defined = {} } },
            .type_id = 0,
            .to = payload_gid,
        } });

        state.results[index].refinement = new_ptr_gid;
        try splat(.errunion_payload_ptr_set, state, index, self);
    }
};

pub fn Simple(comptime instr: anytype) type {
    return struct {
        pub fn apply(self: @This(), state: State, index: usize) !void {
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
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
            _ = self;
            const allocator = state.refinements.list.allocator;

            // Create two scalar fields (result and overflow flag)
            const field0_gid = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
            const field1_gid = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });

            // clobberInst takes ownership of fields slice
            const fields = allocator.alloc(Gid, 2) catch @panic("out of memory");
            fields[0] = field0_gid;
            fields[1] = field1_gid;

            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .@"struct" = .{ .fields = fields, .type_id = 0 } });
            try splat(instr, state, index, .{});
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
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

/// Tag injected at the start of each switch case body.
/// Similar to CondBr but for switch cases - identifies which case this is.
pub const SwitchBr = struct {
    /// Which case this is (0-indexed). The else case is the last index.
    case_index: usize,
    /// Total number of cases including else (for analysis context).
    num_cases: usize,
    /// If switching on a union tag, this contains the union and variant info.
    /// Used by variant_safety to know which variant is active in this case.
    union_tag: ?UnionTagCheck = null,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = index;
        try splat(.switch_br, state, 0, self);
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
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

        // Union fields use ?Gid - null means inactive, some means active
        const fields = allocator.alloc(?Gid, union_fields.len) catch @panic("out of memory");

        for (union_fields, 0..) |_, i| {
            if (i == self.field_index) {
                // Active field - create entity from init value
                switch (self.init) {
                    .inst => |src| {
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

/// Entity operation: TRANSFER (from region's uniform element)
/// ArrayElemVal extracts a value from an array/region at a given index.
/// With uniform regions, all indices share the same element entity.
pub const ArrayElemVal = struct {
    /// Base instruction containing the array/region
    base: ?usize,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const base = self.base orelse {
            // Interned/global array - create a scalar result
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
            try splat(.array_elem_val, state, index, self);
            return;
        };

        const base_ref = state.results[base].refinement orelse {
            // No refinement on base - create scalar fallback
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
            try splat(.array_elem_val, state, index, self);
            return;
        };

        const base_refinement = state.refinements.at(base_ref).*;
        switch (base_refinement) {
            .region => |r| {
                // Uniform region: semideep copy the element
                // Need semideep copy because structs/unions have allocated fields
                const new_gid = try state.refinements.semideepCopy(r.to);
                state.results[index].refinement = new_gid;
            },
            else => {
                // Not a region - fallback to scalar
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{ .type_id = 0 } });
            },
        }
        try splat(.array_elem_val, state, index, self);
    }
};

/// Get a pointer to an array/region element.
/// For uniform regions, returns a pointer to the shared element entity.
pub const PtrElemPtr = struct {
    /// Base instruction containing the pointer to array/region
    base: ?usize,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        const base = self.base orelse {
            // Interned/global pointer - create a pointer to scalar result
            const pointee_idx = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
            try splat(.ptr_elem_ptr, state, index, self);
            return;
        };

        const base_ref = state.results[base].refinement orelse {
            // No refinement on base - create pointer to scalar fallback
            const pointee_idx = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
            try splat(.ptr_elem_ptr, state, index, self);
            return;
        };

        // base should be a pointer to a region
        const base_refinement = state.refinements.at(base_ref).*;
        switch (base_refinement) {
            .pointer => |p| {
                // Follow pointer to get the region
                const pointee = state.refinements.at(p.to).*;
                switch (pointee) {
                    .region => |r| {
                        // Uniform region: create pointer to the shared element
                        // The pointer points to the SAME element entity (r.to)
                        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = r.to } });
                    },
                    else => {
                        // Not a region - fallback to pointer to scalar
                        const pointee_idx = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
                        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
                    },
                }
            },
            else => {
                // Not a pointer - fallback to pointer to scalar
                const pointee_idx = try state.refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .type_id = 0, .to = pointee_idx } });
            },
        }
        try splat(.ptr_elem_ptr, state, index, self);
    }
};

pub const AnyTag = union(enum) {
    // Noop for unreferenced instructions
    noop: Noop,

    // Implemented tags
    alloc: Alloc,
    alloc_create: AllocCreate,
    alloc_destroy: AllocDestroy,
    mkallocator: MkAllocator,
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
    switch_br: SwitchBr,
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
    array_elem_val: ArrayElemVal,
    ptr_elem_ptr: PtrElemPtr,
    sub_with_overflow: OverflowOp(.sub_with_overflow),
    @"try": UnwrapErrunionPayload, // try extracts payload from error union, same as unwrap_errunion_payload
    unreach: Unreach,
    unwrap_errunion_err: Simple(.unwrap_errunion_err), // produces error scalar
    errunion_payload_ptr_set: ErrunionPayloadPtrSet, // *(E!T) -> *T
    wrap_errunion_err: Unimplemented(.{}),
    wrap_errunion_payload: WrapErrunionPayload,

    // Union tags - variant safety not implemented yet
    set_union_tag: SetUnionTag,
    get_union_tag: GetUnionTag,
    union_init: UnionInit,

    // Enum safety check - produces a bool scalar for cond_br
    is_named_enum_value: Simple(.is_named_enum_value),
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

/// Initialize a return slot refinement. Called after creating the return slot
/// to set up initial analysis state (e.g., marking values as defined).
/// Each analysis can implement `retval_init` to initialize its state.
pub fn splatInit(refinements: *Refinements, gid: Gid, ctx: *Context) void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "retval_init")) {
            Analysis.retval_init(refinements, gid, ctx);
        }
    }
}

/// Initialize a return slot refinement as DEFINED. Used for interned comptime values
/// (constants, null) that are not the .undefined type wrapper.
/// Calls retval_init_defined if available (marks as defined), otherwise retval_init.
pub fn splatInitDefined(refinements: *Refinements, gid: Gid, ctx: *Context) void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "retval_init_defined")) {
            Analysis.retval_init_defined(refinements, gid);
        } else if (@hasDecl(Analysis, "retval_init")) {
            Analysis.retval_init(refinements, gid, ctx);
        }
    }
}

/// Merge early returns into the current refinements.
/// Each early_return is a state snapshot at a return point.
/// Called at function end to produce the final merged state for all entities.
/// This is essential for in-out parameters: if one return path sets a value
/// and another doesn't, the merged result should be "conflicting".
/// Only merges entities with GID < base_gid (caller-owned entities).
pub fn splatMergeEarlyReturns(
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    early_returns: []const State,
    base_gid: Gid,
) !void {
    const allocator = ctx.allocator;

    // Build clean States with current results and no dangling pointers
    // The early_returns may have stale results/branch_returns pointers
    const branches = try allocator.alloc(State, early_returns.len);
    defer allocator.free(branches);

    for (early_returns, 0..) |er, i| {
        branches[i] = State{
            .ctx = ctx,
            .results = results, // Use current results, not stale pointer
            .refinements = er.refinements, // Use the cloned refinements
            .return_gid = er.return_gid,
            // Don't copy branch_returns - it may point to freed stack memory
            // branchIsUnreachable will skip the check when null
        };
    }

    try splatMerge(.early_return, results, ctx, refinements, branches, base_gid);

    // Also merge the return value from all early_returns.
    // The return_gid is not in the results table (ret_safe sets result to void),
    // so splatMerge won't find it. We need to handle it separately.
    if (early_returns.len > 0) {
        const return_gid = early_returns[0].return_gid;

        // Build branch_gids array - all branches use the same return_gid
        const branch_gids = try allocator.alloc(?Gid, branches.len);
        defer allocator.free(branch_gids);
        for (branches, 0..) |_, i| {
            branch_gids[i] = return_gid;
        }

        // Build filtered branches (null for unreachable)
        const filtered = try allocator.alloc(?State, branches.len);
        defer allocator.free(filtered);
        for (branches, 0..) |branch, i| {
            filtered[i] = if (branchIsUnreachable(branch)) null else branch;
        }

        // Use a separate merged set for return value
        var merged = std.AutoHashMap(Gid, void).init(allocator);
        defer merged.deinit();

        try mergeRefinementRecursive(
            .early_return,
            allocator,
            ctx,
            refinements,
            return_gid,
            filtered,
            branch_gids,
            &merged,
        );
    }
}

/// Recursively copy analytes from src to dst refinements, following both structures in parallel.
/// dst_gid and src_gid are corresponding entities in the two tables (same position in type structure).
fn copyAnalytesRecursive(dst_refinements: *Refinements, src_refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
    const src_ref = src_refinements.at(src_gid);
    const dst_ref = dst_refinements.at(dst_gid);

    switch (src_ref.*) {
        .scalar => |s| {
            dst_ref.scalar.analyte = s.analyte;
        },
        .pointer => |p| {
            dst_ref.pointer.analyte = p.analyte;
            // Recurse to pointee
            copyAnalytesRecursive(dst_refinements, src_refinements, dst_ref.pointer.to, p.to);
        },
        .optional => |o| {
            dst_ref.optional.analyte = o.analyte;
            // Recurse to inner value
            copyAnalytesRecursive(dst_refinements, src_refinements, dst_ref.optional.to, o.to);
        },
        .errorunion => |e| {
            dst_ref.errorunion.analyte = e.analyte;
            // Recurse to payload
            copyAnalytesRecursive(dst_refinements, src_refinements, dst_ref.errorunion.to, e.to);
        },
        .@"struct" => |s| {
            dst_ref.@"struct".analyte = s.analyte;
            // Recurse to each field
            for (dst_ref.@"struct".fields, s.fields) |dst_field, src_field| {
                copyAnalytesRecursive(dst_refinements, src_refinements, dst_field, src_field);
            }
        },
        .@"union" => |u| {
            dst_ref.@"union".analyte = u.analyte;
            // Recurse to each field (if initialized)
            for (dst_ref.@"union".fields, u.fields) |dst_field_opt, src_field_opt| {
                const dst_field = dst_field_opt orelse continue;
                const src_field = src_field_opt orelse continue;
                copyAnalytesRecursive(dst_refinements, src_refinements, dst_field, src_field);
            }
        },
        .void, .unimplemented, .noreturn, .region => {},
    }
}

/// Called for each orphaned entity detected during branch merge.
/// An orphaned entity is one that was created during branch execution but
/// is no longer reachable from any result slot after the branch completes.
/// Analysis modules can implement `orphaned` to handle these (e.g., detect leaked allocations).
fn splatOrphaned(ctx: *Context, refinements: *Refinements, branch_refinements: *Refinements, gid: Gid) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "orphaned")) {
            try Analysis.orphaned(ctx, refinements, branch_refinements, gid);
        }
    }
}

/// Check if a branch is "unreachable" for post-branch code merge purposes.
/// A branch is unreachable if:
/// - It returned (branch_returns is true)
/// - It contains a .noreturn result (panic, unreachable, etc.)
///
/// Note: When branch_returns is null (e.g., for early_returns merging),
/// we skip the noreturn check since the results may not correspond to
/// the branch's refinements table.
fn branchIsUnreachable(branch: State) bool {
    // Check if branch returned
    if (branch.branch_returns) |br| {
        if (br.*) return true;
    } else {
        // No branch_returns tracking - this is likely an early_returns context
        // where results and refinements may not match. Skip noreturn check.
        return false;
    }

    // Check for noreturn refinement (panic, unreachable, etc.)
    for (branch.results) |result| {
        const eidx = result.refinement orelse continue;
        if (branch.refinements.at(eidx).* == .noreturn) {
            return true;
        }
    }
    return false;
}

/// Merge branch results after a conditional or switch.
/// Walks through all result slots and calls each analysis's merge function.
/// Analysis modules can implement `merge` to handle their specific fields.
///
/// Multiple result slots may reference the same GID internally. We track
/// which GID values have been merged to avoid re-merging the same entity.
/// If merge_base_gid is non-null, only merge entities with GID < merge_base_gid.
/// This is used by early_returns merging to skip callee-owned entities.
pub fn splatMerge(
    comptime merge_tag: anytype,
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    branches: []const State,
    merge_base_gid: ?Gid,
) !void {
    const allocator = ctx.allocator;

    // Record base_len before merge - entities >= this index were created by branches
    const base_len = refinements.list.items.len;

    // Build filtered branch array - null for unreachable
    const filtered = try allocator.alloc(?State, branches.len);
    defer allocator.free(filtered);

    for (branches, 0..) |branch, i| {
        filtered[i] = if (branchIsUnreachable(branch))
            null
        else
            branch;
    }

    // Track merged GIDs to avoid re-merging the same entity
    var merged = std.AutoHashMap(Gid, void).init(allocator);
    defer merged.deinit();

    // Reusable array for branch GID values
    const branch_gids = try allocator.alloc(?Gid, filtered.len);
    defer allocator.free(branch_gids);

    // Walk through all result slots
    for (results, 0..) |*result, result_idx| {
        const orig_gid = result.refinement orelse continue;

        // For early_returns merging, skip callee-owned entities (GID >= base_gid).
        // Different return paths naturally have different values for return values
        // and local variables - that's not an error.
        if (merge_base_gid) |base_gid| {
            if (orig_gid >= base_gid) continue;
        }

        // Build branch GID array for this result slot
        // Check bounds: early_returns may have fewer entities than current refinements
        for (filtered, 0..) |branch_opt, i| {
            if (branch_opt) |branch| {
                if (branch.results[result_idx].refinement) |gid| {
                    // Only include if GID exists in this branch's refinements
                    if (gid < branch.refinements.list.items.len) {
                        branch_gids[i] = gid;
                    } else {
                        branch_gids[i] = null; // Entity created after this return
                    }
                } else {
                    branch_gids[i] = null;
                }
            } else {
                branch_gids[i] = null;
            }
        }

        // Check if branches changed the entity GID for this result slot.
        // If only reachable branches have a consistent entity GID different from orig,
        // update the parent's result to use that entity (e.g., br overwrites block result).
        var consistent_branch_gid: ?Gid = null;
        var all_same = true;
        for (branch_gids) |gid_opt| {
            const gid = gid_opt orelse continue; // Skip unreachable branches
            if (consistent_branch_gid) |prev| {
                if (prev != gid) {
                    all_same = false;
                    break;
                }
            } else {
                consistent_branch_gid = gid;
            }
        }
        if (all_same) {
            if (consistent_branch_gid) |new_gid| {
                if (new_gid != orig_gid) {
                    // All reachable branches have same entity, different from orig.
                    // Update parent's result slot to use the branch's entity.
                    result.refinement = new_gid;
                }
            }
        }

        // Recursively merge this refinement and all nested refinements
        // Use the (possibly updated) result.refinement as the merge target
        try mergeRefinementRecursive(
            merge_tag,
            allocator,
            ctx,
            refinements,
            result.refinement.?,
            filtered,
            branch_gids,
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

        for (base_len..branch_len) |gid_usize| {
            const gid: Gid = @intCast(gid_usize);
            // If not in merged set, it's orphaned (unreachable from results)
            if (!merged.contains(gid)) {
                try splatOrphaned(ctx, refinements, branch.refinements, gid);
            }
        }
    }
}

/// Follow .to field for each branch's gid, updating branch_gids in place.
fn followBranchGids(
    comptime ref_tag: std.meta.Tag(Refinement),
    branches: []const ?State,
    branch_gids: []?Gid,
) void {
    for (branches, branch_gids) |branch_opt, *gid| {
        const branch = branch_opt orelse continue;
        const current_gid = gid.* orelse continue;
        gid.* = @field(branch.refinements.at(current_gid).*, @tagName(ref_tag)).to;
    }
}

/// Recursively merge a refinement and its nested refinements.
/// Calls analysis merge functions at each node, then recurses into children.
fn mergeRefinementRecursive(
    comptime merge_tag: anytype,
    allocator: std.mem.Allocator,
    ctx: *Context,
    refinements: *Refinements,
    orig_gid: Gid,
    branches: []const ?State,
    branch_gids: []?Gid,
    merged: *std.AutoHashMap(Gid, void),
) !void {
    // Skip if we've already merged this entity
    if (merged.contains(orig_gid)) return;
    try merged.put(orig_gid, {});

    // Call each analyzer's merge for this node
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "merge")) {
            try Analysis.merge(
                ctx,
                merge_tag,
                refinements,
                orig_gid,
                branches,
                branch_gids,
            );
        }
    }

    // Recurse into children based on refinement type
    const orig_ref = refinements.at(orig_gid);
    switch (orig_ref.*) {
        // Pointer: check rejection logic before following and merging
        .pointer => |p| {
            // TODO: pointer rejection logic (all same original OR all new, not mixed)
            followBranchGids(.pointer, branches, branch_gids);
            try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, p.to, branches, branch_gids, merged);
        },
        // For optional/errorunion: follow .to in all branches
        // Types always match when following the same structural path
        // If branch exists, gid exists (they're always in sync)
        inline .optional, .errorunion => |data, tag| {
            followBranchGids(tag, branches, branch_gids);
            try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, data.to, branches, branch_gids, merged);
        },
        .@"struct" => |s| {
            // Need separate array for struct fields since we recurse multiple times
            const field_gids = try allocator.alloc(?Gid, branches.len);
            defer allocator.free(field_gids);
            for (s.fields, 0..) |field_idx, field_i| {
                for (branches, branch_gids, 0..) |branch_opt, branch_gid_opt, i| {
                    const branch = branch_opt orelse {
                        field_gids[i] = null;
                        continue;
                    };
                    const branch_gid = branch_gid_opt orelse {
                        field_gids[i] = null;
                        continue;
                    };
                    field_gids[i] = branch.refinements.at(branch_gid).@"struct".fields[field_i];
                }
                try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, field_idx, branches, field_gids, merged);
            }
        },
        .@"union" => |u| {
            // Need separate array for union fields since we recurse multiple times
            const field_gids = try allocator.alloc(?Gid, branches.len);
            defer allocator.free(field_gids);
            for (u.fields, 0..) |field_opt, field_i| {
                // Build branch GIDs for this field
                for (branches, branch_gids, 0..) |branch_opt, branch_gid_opt, i| {
                    const branch = branch_opt orelse {
                        field_gids[i] = null;
                        continue;
                    };
                    const branch_gid = branch_gid_opt orelse {
                        field_gids[i] = null;
                        continue;
                    };
                    field_gids[i] = branch.refinements.at(branch_gid).@"union".fields[field_i];
                }

                // Get or create field entity in original
                const field_idx = if (field_opt) |idx|
                    idx
                else blk: {
                    // Original field is null - find first branch with populated field and copy structure
                    for (branches, field_gids) |branch_opt, branch_field_gid_opt| {
                        const branch = branch_opt orelse continue;
                        const branch_field_gid = branch_field_gid_opt orelse continue;
                        // Cross-table copy: use Refinement.copyTo which properly handles
                        // pointer .to fields by recursively copying pointees
                        const branch_field_ref = branch.refinements.at(branch_field_gid).*;
                        const new_idx = try Refinement.copyTo(branch_field_ref, branch.refinements, refinements);
                        // NOTE: copyTo may reallocate, so re-fetch the union's fields pointer
                        refinements.at(orig_gid).@"union".fields[field_i] = new_idx;
                        break :blk new_idx;
                    }
                    // No branch has this field populated - skip
                    continue;
                };

                try mergeRefinementRecursive(merge_tag, allocator, ctx, refinements, field_idx, branches, field_gids, merged);
            }
        },
        else => {},
    }
}

// Tests

/// Test helper to create a State for testing.
/// Context owns nothing - refinements are passed separately.
fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_gid = 0,
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
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .type_id = 0 } });
    try std.testing.expect(results[1].name_id == null);

    // dbg_var_val should set the name_id on the target instruction
    try Inst.apply(state, 2, .{ .dbg_var_val = .{ .ptr = 1, .name_id = 2 } });

    try std.testing.expectEqual(@as(u32, 2), results[1].name_id.?);
}

test "arg shares eidx from caller (global refinements)" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &testGetName;

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // With global refinements, the caller's entity is already in refinements
    const arg_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // arg should share the entity (same gid), not copy
    try Inst.apply(state, 0, .{ .arg = .{ .value = .{ .inst = arg_gid }, .name_id = 3 } });

    // Should have the same gid as the caller's entity
    try std.testing.expectEqual(arg_gid, results[0].refinement.?);
    // name_id is in the inst_tag
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
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{ .type_id = 0 } });
    const src_gid = results[1].refinement.?;

    // br should share the source refinement with the block
    try Inst.apply(state, 2, .{ .br = .{ .block = 0, .src = .{ .inst = 1 } } });

    // Block should now have the same refinement as source
    try std.testing.expectEqual(src_gid, results[0].refinement.?);
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
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{
        .analyte = .{ .null_safety = .{ .non_null = .{
            .function = "test",
            .file = "test.zig",
            .line = 1,
        } } },
        .type_id = 0,
        .to = inner_gid,
    } });
    results[0].refinement = opt_gid;

    // optional_payload should extract the inner value
    try Inst.apply(state, 1, .{ .optional_payload = .{ .src = .{ .inst = 0 } } });

    // Result should point to the inner scalar
    try std.testing.expectEqual(inner_gid, results[1].refinement.?);
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
    const payload_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const eu_gid = try refinements.appendEntity(.{ .errorunion = .{ .type_id = 0, .to = payload_gid } });
    results[0].refinement = eu_gid;

    // unwrap_errunion_payload should extract the payload
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });

    // Result should point to the payload
    try std.testing.expectEqual(payload_gid, results[1].refinement.?);
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
    const field0_eidx = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const field1_eidx = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const fields = try allocator.alloc(Gid, 2);
    fields[0] = field0_eidx;
    fields[1] = field1_eidx;
    const struct_eidx = try refinements.appendEntity(.{ .@"struct" = .{
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_eidx = try refinements.appendEntity(.{ .pointer = .{ .type_id = 0, .to = struct_eidx } });
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
    const fields = try allocator.alloc(Gid, 2);
    fields[0] = field0_eidx;
    fields[1] = field1_eidx;
    const struct_eidx = try refinements.appendEntity(.{ .@"struct" = .{
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
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .type_id = 0, .to = inner_gid } });
    results[0].refinement = opt_gid;

    // is_non_null should record the check
    try Inst.apply(state, 1, .{ .is_non_null = .{ .src = .{ .inst = 0 } } });

    // The optional's null_safety should be set to unknown with check info
    const opt_ref = refinements.at(opt_gid);
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
    const inner_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const opt_gid = try refinements.appendEntity(.{ .optional = .{ .type_id = 0, .to = inner_gid } });
    results[0].refinement = opt_gid;

    // is_null should record the check
    try Inst.apply(state, 1, .{ .is_null = .{ .src = .{ .inst = 0 } } });

    // The optional's null_safety should be set to unknown with check info
    const opt_ref = refinements.at(opt_gid);
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
    const field0_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const field1_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const fields = try allocator.alloc(?Gid, 2);
    fields[0] = field0_gid;
    fields[1] = field1_gid;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
    } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .type_id = 0, .to = union_gid } });
    results[0].refinement = ptr_gid;

    // set_union_tag should update the active variant
    try Inst.apply(state, 1, .{ .set_union_tag = .{ .ptr = 0, .field_index = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // The union's active field should be set (field 1), others null
    const union_ref = refinements.at(union_gid);
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
    const field_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    const fields = try allocator.alloc(?Gid, 1);
    fields[0] = field_gid;
    const union_gid = try refinements.appendEntity(.{ .@"union" = .{
        .fields = fields,
        .type_id = 0,
    } });
    results[0].refinement = union_gid;

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
    const val_gid = try refinements.appendEntity(.{ .scalar = .{ .type_id = 0 } });
    results[0].refinement = val_gid;

    // union_init should create a union with active variant set
    try Inst.apply(state, 1, .{ .union_init = .{
        .ty = .{ .id = null, .ty = .{ .@"union" = &.{ .{ .id = null, .ty = .{ .scalar = {} } }, .{ .id = null, .ty = .{ .scalar = {} } } } } },
        .field_index = 1,
        .init = .{ .inst = 0 },
    } });

    try std.testing.expect(results[1].refinement != null);
    const union_ref = refinements.at(results[1].refinement.?);
    try std.testing.expectEqual(.@"union", std.meta.activeTag(union_ref.*));
    // Field 1 should be active (non-null), field 0 inactive (null)
    try std.testing.expect(union_ref.@"union".fields[0] == null);
    try std.testing.expect(union_ref.@"union".fields[1] != null);
}
