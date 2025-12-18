const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

block: usize,
src: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.br, tracked, index, ctx, self);
}
