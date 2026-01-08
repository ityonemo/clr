// these exports comprise all the types directly used by the generated code.
pub const Context = @import("Context.zig");
pub const Inst = @import("Inst.zig");
pub const Refinements = @import("Refinements.zig");
pub const GlobalDef = Refinements.GlobalDef;
pub const Src = @import("tag.zig").Src;
pub const Type = @import("tag.zig").Type;
pub const typeToRefinement = @import("tag.zig").typeToRefinement;
pub const splatInit = @import("tag.zig").splatInit;
pub const splatInitGlobal = @import("tag.zig").splatInitGlobal;
pub const GlobalLocation = @import("tag.zig").GlobalLocation;
pub const Gid = Refinements.Gid;
const std = @import("std");

// Debug utilities
/// Dump analysis state. Call as: clr.dump(state);
pub const dump = @import("dump.zig").dump;

/// wraps all state into a single object that is easily passed between units.
/// With global refinements table, State is simpler - no per-function refinements.
pub const State = struct {
    ctx: *Context,
    results: []Inst,
    refinements: *Refinements,
    return_gid: Gid,
    /// Initial refinements length at function entry.
    /// Used by mergeEarlyReturns to skip callee-owned entities (return values, locals)
    /// and only merge caller-owned entities (arguments, passed-in pointers).
    base_gid: Gid = 0,
    /// For branch tracking: GIDs of entities created during this branch (null outside branches)
    created_gids: ?*std.ArrayListUnmanaged(Gid) = null,
    /// For branch tracking: GIDs of entities modified during this branch (null outside branches)
    modified_gids: ?*std.ArrayListUnmanaged(Gid) = null,
    /// Set to true by ret_safe/ret_load when this branch returns.
    /// Used to exclude returning branches from local variable merge.
    /// Null outside of branch contexts (top-level function execution).
    branch_returns: ?*bool = null,
    /// Accumulated early returns - full state snapshots at return points.
    /// Used for hoisting return merging to function end instead of immediately after branches.
    /// Each State contains a heap-allocated Refinements clone for ownership transfer.
    /// Null in test contexts that don't test return handling.
    early_returns: ?*std.ArrayListUnmanaged(State) = null,
};

test {
    @import("std").testing.refAllDecls(@This());
}
