// these exports comprise all the types directly used by the generated code.
pub const Context = @import("Context.zig");
pub const Inst = @import("Inst.zig");
pub const Refinements = @import("Refinements.zig");
pub const EIdx = Inst.EIdx;

// Debug utilities
/// Dump analysis state. Call as: clr.dump(results, ctx, &refinements, caller_refinements, return_eidx);
pub const dump = @import("dump.zig").dump;

test {
    @import("std").testing.refAllDecls(@This());
}
