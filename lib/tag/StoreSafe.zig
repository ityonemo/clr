const Slot = @import("../slots.zig").Slot;

ptr: ?usize,
is_undef: bool,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = index;
    _ = ctx;
    const ptr = self.ptr orelse return;
    if (self.is_undef) {
        tracked[ptr].undefined = .{ .undefined = .{} };
    } else {
        tracked[ptr].undefined = .{ .defined = {} };
    }
}
