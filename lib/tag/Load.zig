const Slot = @import("../slots.zig").Slot;

ptr: ?usize,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = index;
    const ptr = self.ptr orelse return;
    const slot = tracked[ptr];
    if (slot.state == .undefined) {
        return ctx.reportUseBeforeAssign(slot.meta);
    }
}
