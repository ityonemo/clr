pub const Unimplemented = @import("tag/Unimplemented.zig");

// Incomplete: add more tags as we encounter them, make them "Unimplemented".
pub const AnyTag = union(enum) {
    alloc: @import("tag/Alloc.zig"),
    arg: @import("tag/Arg.zig"),
    store_safe: @import("tag/StoreSafe.zig"),
    load: @import("tag/Load.zig"),
    dbg_stmt: @import("tag/DbgStmt.zig"),
    ret_safe: @import("tag/RetSafe.zig"),
    noop_pruned_debug: Unimplemented,
    // Unimplemented tags encountered during integration tests
    dbg_var_ptr: Unimplemented,
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
const analyses = .{Undefined};

pub fn splat(comptime tag: anytype, tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
    inline for (analyses) |Analysis| {
        if (Analysis.implements(tag)) {
            try @field(Analysis, @tagName(tag))(tracked, index, ctx, payload);
        }
    }
}