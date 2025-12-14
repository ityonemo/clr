const Slot = @import("../slots.zig").Slot;

value: Slot,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    tracked[index] = self.value;
    try splat(.arg, tracked, index, ctx, self);
}
