const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

ptr: ?usize,
is_undef: bool,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.store_safe, tracked, index, ctx, self);
}
