const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

retval_ptr: *Slot,
src: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.ret_safe, tracked, index, ctx, self);
}
