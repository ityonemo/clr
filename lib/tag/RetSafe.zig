const Slot = @import("../slots.zig").Slot;

retval_ptr: *Slot,
src: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = index;
    _ = ctx;
    if (self.src) |src| {
        self.retval_ptr.* = tracked[src];
    }
}
