const Inst = @import("Inst.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const EIdx = Inst.EIdx;
const Context = @import("Context.zig");
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
pub const analyses = .{ Undefined, MemorySafety };

// Type

/// This struct exists to propagate information from the AIR generator into
/// the parameters of a instruction.  Some operations are expected to set
/// the type based on interned information; in those cases, the type will be used.
pub const Type = union(enum) {
    scalar: void,
    pointer: *Type,
    optional: *Type,
    region: *Type, // unused, for now, will represent slices (maybe)
    @"struct": void, // unused, for now, temporarily void. Will be a slice of Simple.
    @"union": void, // unused, for now, temporarily void. Will be a slice of Simple.
    void: void,
};

/// Source reference for instructions - indicates where a value comes from.
/// Used by store, br, ret_safe, and other tags that reference source values.
pub const Src = union(enum) {
    /// Runtime value from a result in the results table
    eidx: usize,
    /// Compile-time known value (interned in InternPool)
    interned: Type,
    /// Other sources (globals, etc.) - currently unimplemented
    other: void,
};

// Tag payload types

pub const Alloc = struct {
    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Create the pointed-to future entity (structure determined by first store)
        const pointee_idx = try refinements.appendEntity(.{ .future = .{ .name = null } });
        // Create pointer entity pointing to the future
        _ = try Inst.clobberInst(refinements, results, index, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });
        try splat(.alloc, results, index, ctx, refinements, self);
    }
};

pub const AllocCreate = struct {
    allocator_type: []const u8,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Create the pointed-to future entity (structure determined by first store)
        const pointee_idx = try refinements.appendEntity(.{ .future = .{ .name = null } });
        // Create pointer entity pointing to the future
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
    /// Source value being bitcast.
    src: Src,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed).
        // TODO: When we add analyte fields to pointer entities, we may need to
        // copy/merge analyte data from the source entity to the destination here.
        switch (self.src) {
            .eidx => |src| results[index].refinement = results[src].refinement,
            .interned, .other => {
                // Comptime/global source - create fresh scalar
                // TODO: use type info from .interned to determine structure
                _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
            },
        }
        try splat(.bitcast, results, index, ctx, refinements, self);
    }
};

/// Block creates a labeled scope that can be the target of `br` (break) instructions.
/// The block's result is a `.future` that gets filled in when a `br` targets it.
pub const Block = struct {
    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = self;
        _ = ctx;
        // Block result is a future - will be filled in by br instruction
        _ = try Inst.clobberInst(refinements, results, index, .{ .future = .{ .name = null } });
    }
};

/// Br (break) transfers control to a block, optionally carrying a value.
/// - `break :blk value` → src is .eidx or .interned with the value
/// - `break :blk` (void) → src is .interned with .void type
pub const Br = struct {
    block: usize,
    /// Value being passed to the block.
    src: Src,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity with block instruction (intraprocedural - no copy needed).
        // TODO: Need a way to merge types/analytes when multiple branches target the same block.
        // Currently we just overwrite, but we should union the analysis states.
        switch (self.src) {
            .eidx => |src| {
                if (results[src].refinement) |src_idx| {
                    results[self.block].refinement = src_idx;
                } else {
                    _ = try Inst.clobberInst(refinements, results, self.block, .{ .scalar = .{} });
                }
            },
            .interned => |ty| {
                // Comptime value - create refinement based on type
                // TODO: use full type info to determine structure
                if (ty == .void) {
                    _ = try Inst.clobberInst(refinements, results, self.block, .void);
                } else {
                    _ = try Inst.clobberInst(refinements, results, self.block, .{ .scalar = .{} });
                }
            },
            .other => _ = try Inst.clobberInst(refinements, results, self.block, .{ .scalar = .{} }),
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
    /// Source optional being unwrapped.
    src: Src,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed)
        switch (self.src) {
            .eidx => |src| results[index].refinement = results[src].refinement,
            .interned, .other => {
                // Comptime/global source - create fresh scalar
                // TODO: use type info from .interned to determine structure
                _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
            },
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
    /// Value being returned. Use .interned with .void for void returns.
    src: Src,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);

        // Copy return value to caller's refinements
        if (self.caller_refinements) |caller_refinements| {
            const return_eidx = self.return_eidx;
            switch (self.src) {
                .eidx => |src| {
                    const src_idx = results[src].refinement orelse @panic("return function requested uninitialized instruction value");
                    if (refinements.at(src_idx).* == .retval_future) @panic("cannot return an unset_retval");
                    switch (caller_refinements.at(return_eidx).*) {
                        .retval_future, .void => {
                            // .void can be overwritten - it's from an error path return
                            // Copy return value from callee to caller's return slot
                            const new_idx = try refinements.at(src_idx).*.copy_to(refinements, caller_refinements);
                            caller_refinements.at(return_eidx).* = caller_refinements.at(new_idx).*;
                        },
                        else => {
                            // With cond_br cloning caller_refinements per branch, this case
                            // should only hit for multiple returns within the same branch.
                            // For now, we keep the first return value (conservative: later
                            // returns might have more refined analysis state).
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
                        // Non-void comptime value - create scalar for now
                        // TODO: use type info to determine structure
                        switch (caller_refinements.at(return_eidx).*) {
                            .retval_future, .void => {
                                caller_refinements.at(return_eidx).* = .{ .scalar = .{} };
                            },
                            else => {},
                        }
                    }
                },
                .other => {
                    // Global/other - create scalar for now
                    switch (caller_refinements.at(return_eidx).*) {
                        .retval_future, .void => {
                            caller_refinements.at(return_eidx).* = .{ .scalar = .{} };
                        },
                        else => {},
                    }
                },
            }
        }

        // Splat runs last - analyses see state after copy is complete
        try splat(.ret_safe, results, index, ctx, refinements, self);

        // Mark all futures as tombstoned - this path is exiting, so any
        // unresolved futures won't be resolved by this branch.
        refinements.tombstoneAllFutures();
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
    /// Source value being stored.
    src: Src,
    is_undef: bool,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        _ = try Inst.clobberInst(refinements, results, index, .void);

        // Transform .future pointee into actual structure based on src
        var pending_var_name: ?[]const u8 = null;
        var pointee_idx: ?Inst.EIdx = null;

        if (self.ptr) |ptr| {
            const ptr_idx = results[ptr].refinement orelse {
                try splat(.store, results, index, ctx, refinements, self);
                return;
            };
            if (refinements.at(ptr_idx).* != .pointer) {
                try splat(.store, results, index, ctx, refinements, self);
                return;
            }
            pointee_idx = refinements.at(ptr_idx).pointer.to;

            if (refinements.at(pointee_idx.?).* == .future) {
                // Extract pending var_name before transforming
                pending_var_name = refinements.at(pointee_idx.?).future.name;

                // Determine structure from src, or default to scalar
                // Can't copy from .future or .retval_future, so default to scalar
                const src_ref: Refinement = blk: {
                    switch (self.src) {
                        .eidx => |src| {
                            if (results[src].refinement) |src_idx| {
                                const ref = refinements.at(src_idx).*;
                                if (ref != .future and ref != .retval_future) {
                                    break :blk ref;
                                }
                            }
                        },
                        .interned => {
                            // TODO: use type info to determine structure
                        },
                        .other => {},
                    }
                    break :blk .{ .scalar = .{} };
                };
                Refinement.clobber_structured_idx(pointee_idx.?, refinements, src_ref, refinements);
            }
        }

        // The ptr instruction's entity is unchanged - we only update the pointee's analysis state
        try splat(.store, results, index, ctx, refinements, self);

        // Apply pending var_name to the undefined state if it was set
        if (pending_var_name) |name| {
            if (pointee_idx) |idx| {
                switch (refinements.at(idx).*) {
                    .scalar => |*s| if (s.undefined) |*u| switch (u.*) {
                        .undefined => |*meta| meta.var_name = name,
                        .inconsistent => |*meta| meta.var_name = name,
                        .defined => {},
                    },
                    .pointer => |*p| if (p.analyte.undefined) |*u| switch (u.*) {
                        .undefined => |*meta| meta.var_name = name,
                        .inconsistent => |*meta| meta.var_name = name,
                        .defined => {},
                    },
                    else => {},
                }
            }
        }
    }
};

/// UnwrapErrunionPayload extracts the success value from an error union (e.g., `try x` or `x catch`).
/// The result shares the source's entity - unwrapping doesn't create a copy of the value,
/// it just accesses what's already there.
pub const UnwrapErrunionPayload = struct {
    /// Source error union being unwrapped.
    src: Src,

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Share source instruction's entity (intraprocedural - no copy needed)
        switch (self.src) {
            .eidx => |src| results[index].refinement = results[src].refinement,
            .interned, .other => {
                // Comptime/global source - create fresh scalar
                // TODO: use type info from .interned to determine structure
                _ = try Inst.clobberInst(refinements, results, index, .{ .scalar = .{} });
            },
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
    aggregate_init: Unimplemented(.{}),
    array_to_slice: Unimplemented(.{}),
    block: Block,
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
    // Walk through all result slots
    for (results, true_results, false_results) |*result, true_result, false_result| {
        // Only merge if the original result has a refinement
        const orig_eidx = result.refinement orelse continue;

        // Get refinement indices from each branch (may be null if not touched)
        const true_eidx = true_result.refinement;
        const false_eidx = false_result.refinement;

        // Trivial cases: one side null, copy the other directly
        // Use index-based version because orig might be .future
        if (true_eidx == null and false_eidx == null) continue;
        if (true_eidx == null) {
            Refinement.clobber_structured_idx(orig_eidx, refinements, false_refinements.at(false_eidx.?).*, false_refinements);
            continue;
        }
        if (false_eidx == null) {
            Refinement.clobber_structured_idx(orig_eidx, refinements, true_refinements.at(true_eidx.?).*, true_refinements);
            continue;
        }

        // Handle .future: if one branch has .future and the other has a resolved value, use the resolved one
        const true_ref = true_refinements.at(true_eidx.?).*;
        const false_ref = false_refinements.at(false_eidx.?).*;
        const true_is_future = true_ref == .future;
        const false_is_future = false_ref == .future;

        if (true_is_future and false_is_future) {
            // Both are .future - nothing to merge, leave as .future
            continue;
        }
        if (true_is_future) {
            // True branch didn't resolve - must be tombstoned (early return)
            if (!true_ref.future.tombstoned) {
                @panic("merging non-tombstoned future with resolved value - branch didn't return but also didn't resolve the future");
            }
            // Use false branch's value since true branch exited
            result.refinement = false_eidx;
            continue;
        }
        if (false_is_future) {
            // False branch didn't resolve - must be tombstoned (early return)
            if (!false_ref.future.tombstoned) {
                @panic("merging non-tombstoned future with resolved value - branch didn't return but also didn't resolve the future");
            }
            // Use true branch's value since false branch exited
            result.refinement = true_eidx;
            continue;
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
