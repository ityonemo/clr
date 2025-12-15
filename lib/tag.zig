pub const Unimplemented = @import("tag/Unimplemented.zig");

// Incomplete: add more tags as we encounter them, make them "Unimplemented".
pub const AnyTag = union(enum) {
    alloc: @import("tag/Alloc.zig"),
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
    br: Unimplemented,
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