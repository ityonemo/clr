const Slot = @import("../slots.zig").Slot;

ptr: ?usize,
is_undef: bool,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = index;
    _ = ctx;
    const ptr = self.ptr orelse return;
    if (self.is_undef) {
        tracked[ptr].state = .undefined;
    } else {
        tracked[ptr].state = .defined;
    }
}
