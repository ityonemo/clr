const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.alloc, tracked, index, ctx, self);
}
