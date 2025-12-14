const Slot = @import("../slots.zig").Slot;

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = self;
    _ = tracked;
    _ = index;
    _ = ctx;
    // No-op for unimplemented tags
}
