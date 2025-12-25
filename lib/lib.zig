// these exports comprise all the types directly used by the generated code.
pub const Context = @import("Context.zig");
pub const Inst = @import("Inst.zig");
pub const Refinements = @import("Refinements.zig");
pub const Arg = @import("tag.zig").Src;
pub const EIdx = Inst.EIdx;

// Debug utilities
/// Dump analysis state. Call as: clr.dump(state);
pub const dump = @import("dump.zig").dump;

/// wraps all state into a single object that is easily passed between units.
pub const State = struct {
    ctx: *Context,
    results: []Inst,
    refinements: *Refinements,
    return_eidx: EIdx,
    caller_refinements: ?*Refinements,
};

test {
    @import("std").testing.refAllDecls(@This());
}
