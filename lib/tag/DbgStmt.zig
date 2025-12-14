const Slot = @import("../slots.zig").Slot;

line: u32,
column: u32,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = tracked;
    _ = index;
    ctx.line = ctx.base_line + self.line + 1;
    ctx.column = self.column;
}
