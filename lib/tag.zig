pub const Unimplemented = @import("tag/Unimplemented.zig");

// Incomplete: add more tags as we encounter them, make them "Unimplemented".
pub const AnyTag = union(enum) {
    alloc: @import("tag/Alloc.zig"),
    alloc_create: @import("tag/AllocCreate.zig"),
    alloc_destroy: @import("tag/AllocDestroy.zig"),
    arg: @import("tag/Arg.zig"),
    store_safe: @import("tag/StoreSafe.zig"),
    load: @import("tag/Load.zig"),
    dbg_stmt: @import("tag/DbgStmt.zig"),
    ret_safe: @import("tag/RetSafe.zig"),
    dbg_var_ptr: @import("tag/DbgVarPtr.zig"),
    dbg_var_val: @import("tag/DbgVarPtr.zig"), // Same handler as dbg_var_ptr
    noop_pruned_debug: Unimplemented,
    bitcast: @import("tag/Bitcast.zig"),
    // Unimplemented tags encountered during integration tests
    block: Unimplemented,
    add_with_overflow: Unimplemented,
    struct_field_val: Unimplemented,
    cmp_eq: Unimplemented,
    cond_br: Unimplemented,
    unreach: Unimplemented,
    br: @import("tag/Br.zig"),
    // Unimplemented tags encountered during allocator investigation
    is_non_err: Unimplemented,
    unwrap_errunion_payload: @import("tag/UnwrapErrunionPayload.zig"),
    unwrap_errunion_err: Unimplemented,
    wrap_errunion_err: Unimplemented,
    wrap_errunion_payload: Unimplemented,
    @"try": Unimplemented,
    ptr_add: Unimplemented,
    array_to_slice: Unimplemented,
    dbg_inline_block: Unimplemented,
    struct_field_ptr_index_0: Unimplemented,
    struct_field_ptr_index_1: Unimplemented,
    struct_field_ptr_index_3: Unimplemented,
    is_non_null: Unimplemented,
    optional_payload: Unimplemented,
    cmp_lte: Unimplemented,
    sub: Unimplemented,
    slice: Unimplemented,
    slice_len: Unimplemented,
    memset_safe: Unimplemented,
    ctz: Unimplemented,
    cmp_gt: Unimplemented,
    intcast: Unimplemented,
    bit_and: Unimplemented,
    sub_with_overflow: Unimplemented,
    ret_addr: Unimplemented,
    dbg_arg_inline: @import("tag/DbgVarPtr.zig"), // Same structure as dbg_var_ptr
};

const Slot = @import("slots.zig").Slot;
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
const analyses = .{ Undefined, MemorySafety };

pub fn splat(comptime tag: anytype, tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, @tagName(tag))) {
            try @field(Analysis, @tagName(tag))(tracked, index, ctx, payload);
        }
    }
}

/// Called at the end of each function to allow analyses to perform final checks.
/// Each analysis can implement `onFinish` to do end-of-function processing
/// (e.g., memory leak detection after all paths have been processed).
pub fn splatFinish(tracked: []Slot, retval: *Slot, ctx: anytype) !void {
    inline for (analyses) |Analysis| {
        if (@hasDecl(Analysis, "onFinish")) {
            try Analysis.onFinish(tracked, retval, ctx);
        }
    }
}