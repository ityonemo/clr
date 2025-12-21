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
    ptr: ?usize,
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
    value: EIdx,
    name: []const u8,
    caller_refinements: ?*Refinements,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        const cp = self.caller_refinements orelse unreachable; // Entrypoint shouldn't have args
        // Copy caller's entity directly (no pointer wrapping - AIR args contain values)
        const local_copy_idx = try refinements.copyEntityRecursive(cp, self.value);
        results[index].refinement = local_copy_idx;
        // Set caller_ref for backward propagation on function close
        results[index].caller_ref = self.value;
        try splat(.arg, results, index, ctx, refinements, self);
    }
};

pub const Bitcast = struct {
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed).
        // TODO: When we add analyte fields to pointer entities, we may need to
        // copy/merge analyte data from the source entity to the destination here.
        if (self.src) |src| {
            results[index].refinement = results[src].refinement;
        } else {
            _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        }
        try splat(.bitcast, results, index, ctx, refinements, self);
    }
};

pub const Br = struct {
    block: usize,
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

pub const DbgVarPtr = struct {
    slot: ?usize,
    name: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        try splat(.dbg_var_ptr, results, index, ctx, refinements, self);
    }
};

pub const Load = struct {
    ptr: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
        try splat(.load, results, index, ctx, refinements, self);
    }
};

pub const OptionalPayload = struct {
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

pub const RetSafe = struct {
    caller_refinements: ?*Refinements,
    return_eidx: EIdx,
    src: ?usize,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        try splat(.ret_safe, results, index, ctx, refinements, self);

        // Copy return value to caller's refinements
        const caller_refinements = self.caller_refinements orelse return; // entrypoint, skip

        const return_eidx = self.return_eidx;
        if (self.src) |src| {
            const src_idx = results[src].refinement orelse @panic("return function requested uninitialized instruction value");
            switch (caller_refinements.at(return_eidx).*) {
                .unset_retval => {
                    try caller_refinements.copyInto(return_eidx, refinements, src_idx);
                },
                else => @panic("merge operation not implemented yet"),
            }
        } else {
            switch (caller_refinements.at(return_eidx).*) {
                .unset_retval => caller_refinements.at(return_eidx).* = .void,
                .void => {},
                else => @panic("void function retval incorrectly set to some value"),
            }
        }
    }
};

/// StoreSafe writes a value through a pointer. The store_safe instruction itself is void.
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
pub const StoreSafe = struct {
    ptr: ?usize,
    src: ?usize,
    is_undef: bool,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);
        // The ptr instruction's entity is unchanged - we only update the pointee's analysis state
        try splat(.store_safe, results, index, ctx, refinements, self);
    }
};

pub const UnwrapErrunionPayload = struct {
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

pub fn Unimplemented(comptime opts: anytype) type {
    const known_void = if (@hasField(@TypeOf(opts), "known_void")) opts.known_void else false;
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
    dbg_var_ptr: DbgVarPtr,
    dbg_var_val: DbgVarPtr, // Same structure as dbg_var_ptr
    dbg_arg_inline: DbgVarPtr, // Same structure as dbg_var_ptr
    load: Load,
    optional_payload: OptionalPayload,
    ret_safe: RetSafe,
    store_safe: StoreSafe,
    unwrap_errunion_payload: UnwrapErrunionPayload,

    // Unimplemented tags (no-op)
    add_with_overflow: Unimplemented(.{}),
    array_to_slice: Unimplemented(.{}),
    bit_and: Unimplemented(.{}),
    block: Unimplemented(.{}),
    cmp_eq: Unimplemented(.{}),
    cmp_gt: Unimplemented(.{}),
    cmp_lte: Unimplemented(.{}),
    cond_br: Unimplemented(.{}),
    ctz: Unimplemented(.{}),
    dbg_inline_block: Unimplemented(.{}),
    intcast: Unimplemented(.{}),
    is_non_err: Unimplemented(.{}),
    is_non_null: Unimplemented(.{}),
    memset_safe: Unimplemented(.{}),
    noop_pruned_debug: Unimplemented(.{}),
    ptr_add: Unimplemented(.{}),
    ret_addr: Unimplemented(.{}),
    ret_load: Unimplemented(.{}),
    ret_ptr: Unimplemented(.{}),
    slice: Unimplemented(.{}),
    slice_len: Unimplemented(.{}),
    stack_trace_frames: Unimplemented(.{}),
    store: Unimplemented(.{}),
    struct_field_ptr_index_0: Unimplemented(.{}),
    struct_field_ptr_index_1: Unimplemented(.{}),
    struct_field_ptr_index_2: Unimplemented(.{}),
    struct_field_ptr_index_3: Unimplemented(.{}),
    struct_field_val: Unimplemented(.{}),
    sub: Unimplemented(.{}),
    sub_with_overflow: Unimplemented(.{}),
    @"try": Unimplemented(.{}),
    unreach: Unimplemented(.{}),
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
