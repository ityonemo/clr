const Slot = @import("../slots.zig").Slot;

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = self;
    _ = ctx;
    tracked[index].undefined = .{ .undefined = .{} };
}
