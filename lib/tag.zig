const Inst = @import("Inst.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const EIdx = Inst.EIdx;
const Context = @import("Context.zig");
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
const analyses = .{ Undefined, MemorySafety };

// Tag payload types

pub const Alloc = struct {
    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Create the pointed-to scalar entity (the stack memory)
        const pointee_idx = try refinements.appendEntity(.{ .scalar = .{} });
        // Create pointer entity pointing to the scalar
        _ = try Inst.clobberInst(refinements, results, index, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });
        try splat(.alloc, results, index, ctx, refinements, self);
    }
};

pub const AllocCreate = struct {
    allocator_type: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Create the pointed-to scalar entity (the allocated memory)
        const pointee_idx = try refinements.appendEntity(.{ .scalar = .{} });
        // Create pointer entity pointing to the scalar
        _ = try Inst.clobberInst(refinements, results, index, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });
        try splat(.alloc_create, results, index, ctx, refinements, self);
    }
};

pub const AllocDestroy = struct {
    /// Index into results[] array for the pointer being freed
    ptr: usize,
    allocator_type: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        try splat(.alloc_destroy, results, index, ctx, refinements, self);
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
    caller_refinements: ?*Refinements,
    // value is an index into the caller_refinements array.
    value: EIdx,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        const cp = self.caller_refinements orelse unreachable; // Entrypoint shouldn't have args
        // Copy caller's entity directly (no pointer wrapping - AIR args contain values)
        const local_copy_idx = try cp.at(self.value).*.copy_to(cp, refinements);
        results[index].refinement = local_copy_idx;
        // Set argument info for backward propagation on function close
        results[index].argument = .{ .caller_ref = self.value, .name = self.name };
        try splat(.arg, results, index, ctx, refinements, self);
    }
};

pub const Bitcast = struct {
    /// Index into results[] array. `null` when source is a comptime value
    /// (no instruction to trace), in which case we create a fresh scalar.
    /// TODO: oWe MAY have to turn this into some sort of union if other bitcast-ing 
    /// options are possible.
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed).
        // TODO: When we add analyte fields to pointer entities, we may need to
        // copy/merge analyte data from the source entity to the destination here.
        if (self.src) |src| {
            results[index].refinement = results[src].refinement;
        } else {
            // Comptime source - create fresh scalar
            _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        }
        try splat(.bitcast, results, index, ctx, refinements, self);
    }
};

/// Br (break) transfers control to a block, optionally carrying a value.
/// - `break :blk value` → src is the instruction producing the value
/// - `break :blk` (void) → src is null, block result is void
pub const Br = struct {
    block: usize,
    /// Index into results[] for the value being passed to the block.
    /// Null for void breaks (no value passed).
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity with block instruction (intraprocedural - no copy needed).
        // TODO: Need a way to merge types/analytes when multiple branches target the same block.
        // Currently we just overwrite, but we should union the analysis states.
        if (self.src) |src| {
            if (results[src].refinement) |src_idx| {
                results[self.block].refinement = src_idx;
            } else {
                _ = try Inst.clobberInst(refinements, results, self.block, .{ .scalar = .{} });
            }
        } else {
            _ = try Inst.clobberInst(refinements, results, self.block, .void);
        }
        try splat(.br, results, index, ctx, refinements, self);
    }
};

// DbgStmt is special: it updates the context's meta line/column for error reporting.
// It does NOT splat because no analysis needs to track debug statements.
pub const DbgStmt = struct {
    line: u32,
    column: u32,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        ctx.meta.line = ctx.base_line + self.line + 1;
        ctx.meta.column = self.column;
    }
};

/// DbgVarPtr associates a variable name with a pointer instruction.
/// Unlike DbgStmt, this IS splatted to analyses because they use the variable
/// name for meaningful error messages (e.g., "variable 'x' used before initialization"
/// instead of "instruction 5 used before initialization").
pub const DbgVarPtr = struct {
    /// Index into results[] array for the pointer. Null when the pointer is comptime.
    ptr: ?usize,
    name: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        try splat(.dbg_var_ptr, results, index, ctx, refinements, self);
    }
};

/// DbgVarVal associates a variable name with a value (non-pointer) instruction.
/// Unlike DbgVarPtr, this is NOT used for stack pointer tracking since values
/// don't have memory locations that can escape. Analyses that only care about
/// pointers (like memory_safety) can ignore this.
pub const DbgVarVal = struct {
    /// Index into results[] array for the value. Null when the value is comptime.
    ptr: ?usize,
    name: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        try splat(.dbg_var_val, results, index, ctx, refinements, self);
    }
};

/// Load dereferences a pointer and produces the value stored at that memory location.
///
/// LIMITATION: Currently always creates a fresh scalar, which is incorrect for
/// pointer-to-pointer types (e.g., `**u8`). Loading from a `**u8` should produce
/// the inner pointer entity, not a fresh scalar. This breaks tracking for multi-level
/// pointer dereferences. See CLAUDE.md "Known Limitations" section.
pub const Load = struct {
    /// Index into results[] array. Null when loading from a global or constant
    /// pointer (interned value with no instruction to trace).
    /// TODO: We'll need to track globals/constants to properly analyze loads from them.
    ptr: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // TODO: Should follow pointer and share pointee's entity, not create fresh scalar
        _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        try splat(.load, results, index, ctx, refinements, self);
    }
};

/// OptionalPayload extracts the inner value from an optional (e.g., `x.?` or `x orelse`).
/// The result shares the source's entity - unwrapping doesn't create a copy of the value,
/// it just accesses what's already there.
pub const OptionalPayload = struct {
    /// Index into results[] array. Null when source is a comptime value
    /// (no instruction to trace), in which case we create a fresh scalar.
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed)
        if (self.src) |src| {
            results[index].refinement = results[src].refinement;
        } else {
            _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        }
        try splat(.optional_payload, results, index, ctx, refinements, self);
    }
};

/// RetSafe returns a value from a function. "Safe" refers to safety-checked returns
/// (as opposed to naked/inline assembly returns).
///
/// This handles copying the return value's entity back to the caller's refinements,
/// enabling interprocedural tracking of returned values.
pub const RetSafe = struct {
    caller_refinements: ?*Refinements,
    return_eidx: EIdx,
    /// Index into results[] array for the value being returned.
    /// Null for void returns (function returns nothing).
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);

        // Copy return value to caller's refinements
        if (self.caller_refinements) |caller_refinements| {
            const return_eidx = self.return_eidx;
            if (self.src) |src| {
                const src_idx = results[src].refinement orelse @panic("return function requested uninitialized instruction value");
                if (refinements.at(src_idx).* == .unset_retval) @panic("cannot return an unset_retval");
                switch (caller_refinements.at(return_eidx).*) {
                    .unset_retval, .void => {
                        // .void can be overwritten - it's from an error path return
                        // Copy return value from callee to caller's return slot
                        const new_idx = try refinements.at(src_idx).*.copy_to(refinements, caller_refinements);
                        caller_refinements.at(return_eidx).* = caller_refinements.at(new_idx).*;
                    },
                    else => {
                        // TODO: implement proper merge for multiple return paths
                    },
                }
            } else {
                switch (caller_refinements.at(return_eidx).*) {
                    .unset_retval => caller_refinements.at(return_eidx).* = .void,
                    .void => {},
                    else => @panic("void function retval incorrectly set to some value"),
                }
            }
        }

        // Splat runs last - analyses see state after copy is complete
        try splat(.ret_safe, results, index, ctx, refinements, self);
    }
};

/// Store writes a value through a pointer. The store instruction itself is void.
/// Used for both `store` and `store_safe` AIR instructions (semantically identical
/// for our analysis - the difference is only runtime safety checks).
///
/// AIR Semantics:
/// - `ptr` is the instruction containing a pointer entity (from alloc, arg, etc.)
/// - `src` is the instruction containing the value to store (may be null for constants)
/// - is_undef indicates whether we're storing an undefined value
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
    /// Index into results[] for the value being stored.
    /// Null when storing a comptime constant.
    src: ?usize,
    is_undef: bool,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        // The ptr instruction's entity is unchanged - we only update the pointee's analysis state
        try splat(.store, results, index, ctx, refinements, self);
    }
};

/// UnwrapErrunionPayload extracts the success value from an error union (e.g., `try x` or `x catch`).
/// The result shares the source's entity - unwrapping doesn't create a copy of the value,
/// it just accesses what's already there.
pub const UnwrapErrunionPayload = struct {
    /// Index into results[] array. Null when source is a comptime value
    /// (no instruction to trace), in which case we create a fresh scalar.
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed)
        if (self.src) |src| {
            results[index].refinement = results[src].refinement;
        } else {
            _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        }
        try splat(.unwrap_errunion_payload, results, index, ctx, refinements, self);
    }
};

pub fn Simple(comptime instr: anytype) type {
    return struct {
        pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
            _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
            try splat(instr, results, index, ctx, refinements, self);
        }
    };
}

pub fn Unimplemented(comptime opts: anytype) type {
    const known_void = if (@hasField(@TypeOf(opts), "void")) opts.void else false;
    return struct {
        pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
            _ = self;
            _ = ctx;
            if (known_void) {
                _ = try Inst.clobberInst(refinements, results, index, .void);
            } else {
                _ = try Inst.clobberInst(refinements, results, index, .unimplemented);
            }
        }
    };
}

pub const AnyTag = union(enum) {
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
    add_with_overflow: Unimplemented(.{}),
    array_to_slice: Unimplemented(.{}),
    block: Unimplemented(.{}),
    cond_br: Unimplemented(.{ .void = true }),
    dbg_inline_block: Unimplemented(.{}),
    intcast: Unimplemented(.{}),
    is_non_err: Unimplemented(.{}),
    is_non_null: Unimplemented(.{}),
    memset_safe: Unimplemented(.{ .void = true }),
    noop_pruned_debug: Unimplemented(.{ .void = true }),
    ptr_add: Unimplemented(.{}),
    ret_addr: Unimplemented(.{}),
    ret_load: Unimplemented(.{}),
    ret_ptr: Unimplemented(.{}),
    slice: Unimplemented(.{}),
    slice_len: Unimplemented(.{}),
    stack_trace_frames: Unimplemented(.{}),
    struct_field_ptr_index_0: Unimplemented(.{}),
    struct_field_ptr_index_1: Unimplemented(.{}),
    struct_field_ptr_index_2: Unimplemented(.{}),
    struct_field_ptr_index_3: Unimplemented(.{}),
    struct_field_val: Unimplemented(.{}),
    sub_with_overflow: Unimplemented(.{}),
    @"try": Unimplemented(.{}),
    unreach: Unimplemented(.{ .void = true }),
    unwrap_errunion_err: Unimplemented(.{}),
    wrap_errunion_err: Unimplemented(.{}),
    wrap_errunion_payload: Unimplemented(.{}),
};

pub fn splat(comptime tag: anytype, results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, payload: anytype) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, @tagName(tag))) {
            try @field(Analysis, @tagName(tag))(results, index, ctx, refinements, payload);
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
