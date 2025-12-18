const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

src: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.unwrap_errunion_payload, tracked, index, ctx, self);
}
