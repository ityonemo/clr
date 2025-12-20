const slots = @import("slots.zig");
const Slot = slots.Slot;
const Payloads = slots.Payloads;
const Context = @import("Context.zig");
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
const analyses = .{ Undefined, MemorySafety };

// Tag payload types

pub const Alloc = struct {
    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.initSlot(tracked, index);
        try splat(.alloc, tracked, index, ctx, payloads, self);
    }
};

pub const AllocCreate = struct {
    allocator_type: []const u8,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.initSlot(tracked, index);
        try splat(.alloc_create, tracked, index, ctx, payloads, self);
    }
};

pub const AllocDestroy = struct {
    ptr: ?usize,
    allocator_type: []const u8,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .void);
        try splat(.alloc_destroy, tracked, index, ctx, payloads, self);
    }
};

// Arg copies slot data from the caller before splatting.
pub const Arg = struct {
    value: *Slot,
    name: []const u8,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        tracked[index] = self.value.*;
        try splat(.arg, tracked, index, ctx, payloads, self);
    }
};

pub const Bitcast = struct {
    src: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .{ .scalar = .{} });
        try splat(.bitcast, tracked, index, ctx, payloads, self);
    }
};

pub const Br = struct {
    block: usize,
    src: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        // TODO: This should be a merge, not a clobber
        _ = try payloads.clobberSlot(tracked, self.block, .{ .scalar = .{} });
        try splat(.br, tracked, index, ctx, payloads, self);
    }
};

// DbgStmt is special: it updates the context's meta line/column for error reporting.
// It does NOT splat because no analysis needs to track debug statements.
pub const DbgStmt = struct {
    line: u32,
    column: u32,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .void);
        ctx.meta.line = ctx.base_line + self.line + 1;
        ctx.meta.column = self.column;
    }
};

pub const DbgVarPtr = struct {
    slot: ?usize,
    name: []const u8,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .void);
        try splat(.dbg_var_ptr, tracked, index, ctx, payloads, self);
    }
};

pub const Load = struct {
    ptr: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .{ .scalar = .{} });
        try splat(.load, tracked, index, ctx, payloads, self);
    }
};

pub const OptionalPayload = struct {
    src: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .{ .scalar = .{} });
        try splat(.optional_payload, tracked, index, ctx, payloads, self);
    }
};

pub const RetSafe = struct {
    retval_ptr: *Slot,
    src: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .void);
        try splat(.ret_safe, tracked, index, ctx, payloads, self);
    }
};

pub const StoreSafe = struct {
    ptr: ?usize,
    src: ?usize,
    is_undef: bool,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .void);
        if (self.ptr) |ptr| _ = try payloads.clobberSlot(tracked, ptr, .{ .scalar = .{} });
        try splat(.store_safe, tracked, index, ctx, payloads, self);
    }
};

pub const UnwrapErrunionPayload = struct {
    src: ?usize,

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        _ = try payloads.clobberSlot(tracked, index, .{ .scalar = .{} });
        try splat(.unwrap_errunion_payload, tracked, index, ctx, payloads, self);
    }
};

pub fn Unimplemented(comptime opts: anytype) type {
    const known_void = if (@hasField(@TypeOf(opts), "known_void")) opts.known_void else false;
    return struct {
        pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
            _ = self;
            _ = ctx;
            if (known_void) {
                _ = try payloads.clobberSlot(tracked, index, .void);
            } else {
                _ = try payloads.clobberSlot(tracked, index, .unimplemented);
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

pub fn splat(comptime tag: anytype, tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, payload: anytype) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, @tagName(tag))) {
            try @field(Analysis, @tagName(tag))(tracked, index, ctx, payloads, payload);
        }
    }
}

/// Called at the end of each function to allow analyses to perform final checks.
/// Each analysis can implement `onFinish` to do end-of-function processing
/// (e.g., memory leak detection after all paths have been processed).
pub fn splatFinish(tracked: []Slot, retval: *Slot, ctx: *Context, payloads: *Payloads) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "onFinish")) {
            try Analysis.onFinish(tracked, retval, ctx, payloads);
        }
    }
}
