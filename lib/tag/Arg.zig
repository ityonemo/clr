const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

value: Slot,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    tracked[index] = self.value;
    tracked[index].reference_arg = index;
    try splat(.arg, tracked, index, ctx, self);
}
