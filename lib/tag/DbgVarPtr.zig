const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

slot: ?usize,
name: []const u8,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    try splat(.dbg_var_ptr, tracked, index, ctx, self);
}
