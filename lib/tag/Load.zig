const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

ptr: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.load, tracked, index, ctx, self);
}
