const Slot = @import("../slots.zig").Slot;

value: Slot,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = ctx;
    tracked[index] = self.value;
}
