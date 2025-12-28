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
pub const analyses = .{ Undefined, MemorySafety, NullSafety, VariantSafety };

// Type

/// Represents a struct or union field with type and optional name.
/// Name is null for tuple fields (anonymous structs).
pub const Field = struct {
    ty: Type,
    name: ?[]const u8 = null,
};

/// This struct exists to propagate information from the AIR generator into
/// the parameters of a instruction.  Some operations are expected to set
/// the type based on interned information; in those cases, the type will be used.
pub const Type = union(enum) {
    scalar: void,
    pointer: *const Type,
    optional: *const Type,
    errorunion: *const Type, // error union payload type
    null: *const Type, // used to signal that an optional is being set to null.  Inner type must be optional.
    undefined: *const Type, // used to signal that the type is undefined, must be scalar, pointer, optional
    region: *const Type, // unused, for now, will represent slices (maybe)
    @"struct": []const Field, // field types and names for struct
    @"union": []const Field, // field types and names for union
    void: void,
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
    return switch (ty) {
        .scalar => .{ .scalar = .{} },
        .void => .void,
        .pointer => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .pointer = .{ .analyte = .{}, .to = child_idx } };
        },
        .optional => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .analyte = .{}, .to = child_idx } };
        },
        .errorunion => |child| {
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .errorunion = .{ .analyte = .{}, .to = child_idx } };
        },
        .null => |child| {
            // .null is a null optional value - creates same structure as .optional
            const child_ref = try typeToRefinement(child.*, refinements);
            const child_idx = try refinements.appendEntity(child_ref);
            return .{ .optional = .{ .analyte = .{}, .to = child_idx } };
        },
        .undefined => |child| {
            // .undefined wraps a type - recurse into inner type.
            // The undefined.store handler checks for .undefined wrapper and marks as undefined.
            return typeToRefinement(child.*, refinements);
        },
        .region => @panic("regions not implemented yet"),
        inline .@"struct", .@"union" => |type_fields, tag| blk: {
            const allocator = refinements.list.allocator;
            const FieldType = if (tag == .@"union") ?Refinements.EIdx else Refinements.EIdx;
            const fields = allocator.alloc(FieldType, type_fields.len) catch @panic("out of memory");
            const field_names = allocator.alloc(?[]const u8, type_fields.len) catch @panic("out of memory");
            for (type_fields, 0..) |field, i| {
                // For structs, create entities for each field; for unions, all fields start inactive (null)
                fields[i] = if (tag == .@"union")
                    null
                else
                    try refinements.appendEntity(try typeToRefinement(field.ty, refinements));
                field_names[i] = field.name;
            }
            break :blk @unionInit(Refinement, @tagName(tag), .{ .fields = fields, .field_names = field_names });
        },
    };
}

// Tag payload types

pub const Alloc = struct {
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create pointee from type info
        const pointee_ref = try typeToRefinement(self.ty, state.refinements);
        const pointee_idx = try state.refinements.appendEntity(pointee_ref);
        // Create pointer entity pointing to the typed pointee
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });
        try splat(.alloc, state, index, self);
    }
};

pub const AllocCreate = struct {
    allocator_type: []const u8,
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // AllocCreate returns Allocator.Error!*T, so we create errorunion -> ptr -> T
        // Create pointee from type info
        const pointee_ref = try typeToRefinement(self.ty, state.refinements);
        const pointee_idx = try state.refinements.appendEntity(pointee_ref);
        // Create pointer entity pointing to the typed pointee
        const ptr_idx = try state.refinements.appendEntity(.{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });
        // Wrap in error union
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .errorunion = .{ .analyte = .{}, .to = ptr_idx } });
        try splat(.alloc_create, state, index, self);
    }
};

pub const AllocDestroy = struct {
    /// Index into results[] array for the pointer being freed
    ptr: usize,
    allocator_type: []const u8,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        try splat(.alloc_destroy, state, index, self);
    }
};

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
    name: []const u8,
    /// Source of the argument value - either runtime (.eidx) or compile-time (.interned)
    value: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        switch (self.value) {
            .eidx => |eidx| {
                const caller_eidx: EIdx = @intCast(eidx);
                const cp = state.caller_refinements orelse unreachable; // Entrypoint shouldn't have args
                // Copy caller's entity directly (no pointer wrapping - AIR args contain values)
                const local_copy_idx = try cp.at(caller_eidx).*.copy_to(cp, state.refinements);
                state.results[index].refinement = local_copy_idx;
                // Set caller_eidx for backward propagation on function close
                state.results[index].caller_eidx = caller_eidx;
                state.results[index].name = self.name;
            },
            .interned => |ty| {
                // Compile-time constant - create entity from type info
                const ref = try typeToRefinement(ty, state.refinements);
                const local_idx = try state.refinements.appendEntity(ref);
                state.results[index].refinement = local_idx;
                // No backward propagation needed for constants (caller_eidx stays null),
                // but still record parameter name for error messages
                state.results[index].name = self.name;
            },
            .other => @panic("Arg: .other source not yet implemented"),
        }
        try splat(.arg, state, index, self);
    }
};

pub const Bitcast = struct {
    /// Source value being bitcast.
    src: Src,
    /// Destination type - available for analysis but bitcast shares source refinement.
    /// Zig's ?*T is represented as a nullable pointer, so we track null_safety on pointers.
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Share source instruction's entity (intraprocedural - no copy needed).
        // For pointer-to-optional coercion (?*T), we keep the pointer refinement
        // and track null_safety on the pointer's analyte.
        switch (self.src) {
            .eidx => |src| state.results[index].refinement = state.results[src].refinement,
            .interned => |ty| {
                // Comptime/global source - create from type
                const ref = try typeToRefinement(ty, state.refinements);
                const ref_idx = try state.refinements.appendEntity(ref);
                state.results[index].refinement = ref_idx;
            },
            .other => {
                // Comptime/global source without type info - create fresh scalar
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
            },
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
    name: []const u8,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        const inst = self.ptr orelse return;
        state.results[inst].name = self.name;
        try splat(.dbg_var_ptr, state, index, self);
    }
};

/// DbgVarVal associates a variable name with a value (non-pointer) instruction.
/// Sets the name on the instruction for use in error messages.
pub const DbgVarVal = struct {
    /// Index into results[] array for the value. Null when the value is comptime.
    ptr: ?usize,
    name: []const u8,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .void);
        const inst = self.ptr orelse return;
        state.results[inst].name = self.name;
    }
};

/// Load dereferences a pointer and produces the value stored at that memory location.
///
/// For compound types (structs, pointers), loading shares the pointee's entity.
/// For scalars, creates a fresh scalar since values are copied.
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
            state.results[index].name = state.results[ptr].name;
            try splat(.load, state, index, self);
            return;
        };

        // Follow pointer to get pointee
        const pointee_idx = switch (state.refinements.at(ptr_ref).*) {
            .pointer => |p| p.to,
            else => {
                _ = try Inst.clobberInst(state.refinements, state.results, index, try typeToRefinement(self.ty, state.refinements));
                state.results[index].name = state.results[ptr].name;
                try splat(.load, state, index, self);
                return;
            },
        };

        // Share the entity directly. This ensures modifications are visible everywhere.
        state.results[index].refinement = pointee_idx;
        // Propagate name from source pointer to load result.
        // When we load a value through a named pointer (e.g., `container.ptr`),
        // the loaded value should inherit that name for error reporting.
        state.results[index].name = state.results[ptr].name;
        try splat(.load, state, index, self);
    }
};

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
                        const new_field_ref = try typeToRefinement(self.ty.pointer.*, state.refinements);
                        const new_idx = try state.refinements.appendEntity(new_field_ref);
                        break :idx new_idx;
                    } else {
                        break :idx data.fields[self.field_index];
                    }
                };
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .to = field_idx } });

                // Build access path: base_name + "." + field_name
                const field_name: ?[]const u8 = if (data.field_names.len > self.field_index)
                    data.field_names[self.field_index]
                else
                    null;
                const base_name = state.results[base].name;
                state.results[index].name = if (base_name) |bn| blk: {
                    if (field_name) |fn_| {
                        break :blk std.fmt.allocPrint(state.ctx.allocator, "{s}.{s}", .{ bn, fn_ }) catch null;
                    }
                    break :blk bn;
                } else field_name;
            },
            else => |t| std.debug.panic("struct_field_ptr: expected struct or union, got {s}", .{@tagName(t)}),
        }

        try splat(.struct_field_ptr, state, index, self);
    }
};

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

                // Build access path: operand_name + "." + field_name
                const field_name: ?[]const u8 = if (data.field_names.len > self.field_index)
                    data.field_names[self.field_index]
                else
                    null;
                const base_name = state.results[operand].name;
                state.results[index].name = if (base_name) |bn| blk: {
                    if (field_name) |fn_| {
                        break :blk std.fmt.allocPrint(state.ctx.allocator, "{s}.{s}", .{ bn, fn_ }) catch null;
                    }
                    break :blk bn;
                } else field_name;
            },
            else => |t| std.debug.panic("struct_field_val: expected struct or union, got {s}", .{@tagName(t)}),
        }
        try splat(.struct_field_val, state, index, self);
    }
};

/// OptionalPayload extracts the inner value from an optional (e.g., `x.?` or `x orelse`).
/// The result shares the source's entity - unwrapping doesn't create a copy of the value,
/// it just accesses what's already there.
pub const OptionalPayload = struct {
    /// Source optional being unwrapped.
    src: Src,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Share source instruction's entity (intraprocedural - no copy needed)
        switch (self.src) {
            .eidx => |src| state.results[index].refinement = state.results[src].refinement,
            .interned, .other => {
                // Comptime/global source - create fresh scalar
                // TODO: use type info from .interned to determine structure
                _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
            },
        }
        try splat(.optional_payload, state, index, self);
    }
};

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
                            const new_idx = try state.refinements.at(src_idx).*.copy_to(state.refinements, caller_refinements);
                            // Deep copy for structs to avoid double-free on deinit
                            const new_val = caller_refinements.at(new_idx).*;
                            caller_refinements.at(return_eidx).* = switch (new_val) {
                                .@"struct" => |s| blk: {
                                    const allocator = caller_refinements.list.allocator;
                                    const new_fields = allocator.alloc(Refinements.EIdx, s.fields.len) catch @panic("out of memory");
                                    @memcpy(new_fields, s.fields);
                                    break :blk .{ .@"struct" = .{ .analyte = s.analyte, .fields = new_fields } };
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
                    if (ty == .void) {
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .pointer = .{ .analyte = .{}, .to = return_idx } });

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
                    const new_idx = try state.refinements.at(pointee_idx).*.copy_to(state.refinements, caller_refinements);
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

/// UnwrapErrunionPayload extracts the success value from an error union (e.g., `try x` or `x catch`).
/// The result shares the source's payload entity - unwrapping doesn't create a copy of the value,
/// it just accesses what's already there.
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
            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
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
            const field0_idx = try state.refinements.appendEntity(.{ .scalar = .{} });
            const field1_idx = try state.refinements.appendEntity(.{ .scalar = .{} });

            // clobberInst takes ownership of fields slice
            const fields = allocator.alloc(EIdx, 2) catch @panic("out of memory");
            fields[0] = field0_idx;
            fields[1] = field1_idx;

            _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .@"struct" = .{ .fields = fields } });
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
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
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
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

        // Deactivate all fields, then create fresh entity for active field
        const fields = state.refinements.at(union_eidx).@"union".fields;
        for (fields) |*field| field.* = null;

        // Create a fresh entity with the correct type for the active field
        // NOTE: This may reallocate the refinements list, so we store by index
        // The Undefined analysis handler will mark this as undefined via splat()
        const field_ref = try typeToRefinement(self.ty, state.refinements);
        fields[field_idx] = try state.refinements.appendEntity(field_ref);

        try splat(.set_union_tag, state, index, self);
    }
};

/// GetUnionTag gets the active tag from a union.
/// Produces a scalar result (the tag value).
pub const GetUnionTag = struct {
    operand: ?usize, // the union instruction we're getting the tag from

    pub fn apply(self: @This(), state: State, index: usize) !void {
        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
        try splat(.get_union_tag, state, index, self);
    }
};

/// UnionInit initializes a union with one active field.
/// Only the specified field gets a real entity; others are null (inactive).
pub const UnionInit = struct {
    field_index: usize,
    init: Src,
    ty: Type,

    pub fn apply(self: @This(), state: State, index: usize) !void {
        // Create union refinement from type info
        const union_fields = self.ty.@"union";
        const allocator = state.refinements.list.allocator;

        // Union fields use ?EIdx - null means inactive, some means active
        const fields = allocator.alloc(?Refinements.EIdx, union_fields.len) catch @panic("out of memory");
        const field_names = allocator.alloc(?[]const u8, union_fields.len) catch @panic("out of memory");

        for (union_fields, 0..) |field, i| {
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
            field_names[i] = field.name;
        }

        _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .@"union" = .{ .fields = fields, .field_names = field_names } });
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
    @"try": Unimplemented(.{}),
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

/// Merge branch results after a conditional.
/// Walks through all result slots and calls each analysis's merge function.
/// Analysis modules can implement `merge` to handle their specific fields.
pub fn splatMerge(
    comptime tag: anytype,
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    true_results: []Inst,
    true_refinements: *Refinements,
    false_results: []Inst,
    false_refinements: *Refinements,
) !void {
    // Check if either branch is unreachable (contains any .noreturn result)
    // If a branch is unreachable, don't merge its state - use only the reachable branch
    const true_is_unreachable = branchIsUnreachable(true_results, true_refinements);
    const false_is_unreachable = branchIsUnreachable(false_results, false_refinements);

    // Walk through all result slots
    for (results, true_results, false_results) |*result, true_result, false_result| {
        // Only merge if the original result has a refinement
        const orig_eidx = result.refinement orelse continue;

        // Get refinement indices from each branch (may be null if not touched)
        const true_eidx = true_result.refinement;
        const false_eidx = false_result.refinement;

        // Handle unreachable branches: if a branch is unreachable (contains noreturn),
        // use only the reachable branch's state without merging
        if (true_is_unreachable and false_is_unreachable) {
            // Both branches unreachable - nothing to merge
            continue;
        }
        if (true_is_unreachable) {
            // True branch is unreachable - use false branch's state only
            if (false_eidx) |fidx| {
                try Refinement.clobber_structured_idx(orig_eidx, refinements, false_refinements.at(fidx).*, false_refinements);
            }
            continue;
        }
        if (false_is_unreachable) {
            // False branch is unreachable - use true branch's state only
            if (true_eidx) |tidx| {
                try Refinement.clobber_structured_idx(orig_eidx, refinements, true_refinements.at(tidx).*, true_refinements);
            }
            continue;
        }

        // If a branch changed which entity this result points to (e.g., br sharing),
        // propagate that to the original. Clone preserves indices, so this is valid.
        // Prefer true branch (success path in error unions).
        if (true_eidx != null and true_eidx.? != orig_eidx) {
            result.refinement = true_eidx.?;
            // Also need to merge analysis state from the branch's entity to the original
            inline for (analyses) |Analysis| {
                if (@hasDecl(Analysis, "merge")) {
                    try Analysis.merge(
                        ctx,
                        tag,
                        .{ refinements, true_eidx.? },
                        .{ true_refinements, true_eidx.? },
                        .{ false_refinements, false_eidx orelse orig_eidx },
                    );
                }
            }
            continue;
        }

        // Trivial cases: one side null, copy the other directly
        // Use index-based version because orig might be .retval_future
        if (true_eidx == null and false_eidx == null) continue;
        if (true_eidx == null) {
            try Refinement.clobber_structured_idx(orig_eidx, refinements, false_refinements.at(false_eidx.?).*, false_refinements);
            continue;
        }
        if (false_eidx == null) {
            try Refinement.clobber_structured_idx(orig_eidx, refinements, true_refinements.at(true_eidx.?).*, true_refinements);
            continue;
        }

        // Handle .retval_future: if one branch has retval_future and the other has a resolved value, use the resolved one
        const true_ref = true_refinements.at(true_eidx.?).*;
        const false_ref = false_refinements.at(false_eidx.?).*;
        const true_is_retval_future = true_ref == .retval_future;
        const false_is_retval_future = false_ref == .retval_future;

        if (true_is_retval_future and false_is_retval_future) {
            // Both are retval_futures - nothing to merge, leave as is
            continue;
        }
        if (true_is_retval_future) {
            // True branch didn't return - use false branch's value
            try Refinement.clobber_structured_idx(orig_eidx, refinements, false_ref, false_refinements);
            continue;
        }
        if (false_is_retval_future) {
            // False branch didn't return - use true branch's value
            try Refinement.clobber_structured_idx(orig_eidx, refinements, true_ref, true_refinements);
            continue;
        }

        // Handle .noreturn: if one branch is unreachable, use the other branch's state
        const true_is_noreturn = true_ref == .noreturn;
        const false_is_noreturn = false_ref == .noreturn;

        if (true_is_noreturn and false_is_noreturn) {
            // Both are noreturn - nothing to merge, leave as is
            continue;
        }
        if (true_is_noreturn) {
            // True branch is unreachable - use false branch's value
            try Refinement.clobber_structured_idx(orig_eidx, refinements, false_ref, false_refinements);
            continue;
        }
        if (false_is_noreturn) {
            // False branch is unreachable - use true branch's value
            try Refinement.clobber_structured_idx(orig_eidx, refinements, true_ref, true_refinements);
            continue;
        }

        // Handle orig being .retval_future when both branches resolved it
        const orig_ref = refinements.at(orig_eidx).*;
        if (orig_ref == .retval_future) {
            // Both branches resolved the retval_future - copy structure from true branch
            // then continue to merge analysis states
            try Refinement.clobber_structured_idx(orig_eidx, refinements, true_ref, true_refinements);
        }

        // Non-trivial: both branches have resolved states - call each analyzer's merge
        inline for (analyses) |Analysis| {
            if (@hasDecl(Analysis, "merge")) {
                try Analysis.merge(
                    ctx,
                    tag,
                    .{ refinements, orig_eidx },
                    .{ true_refinements, true_eidx.? },
                    .{ false_refinements, false_eidx.? },
                );
            }
        }
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

test "dbg_var_ptr sets name on target instruction" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc to create a pointer
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try std.testing.expect(results[1].name == null);

    // dbg_var_ptr should set the name on the target instruction
    try Inst.apply(state, 2, .{ .dbg_var_ptr = .{ .ptr = 1, .name = "my_var" } });

    try std.testing.expectEqualStrings("my_var", results[1].name.?);
}

test "dbg_var_ptr with null ptr does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // dbg_var_ptr with null ptr should not crash
    try Inst.apply(state, 1, .{ .dbg_var_ptr = .{ .ptr = null, .name = "my_var" } });
}

test "dbg_var_val sets name on target instruction" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Set up a result at index 1
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .scalar = .{} });
    try std.testing.expect(results[1].name == null);

    // dbg_var_val should set the name on the target instruction
    try Inst.apply(state, 2, .{ .dbg_var_val = .{ .ptr = 1, .name = "my_val" } });

    try std.testing.expectEqualStrings("my_val", results[1].name.?);
}
