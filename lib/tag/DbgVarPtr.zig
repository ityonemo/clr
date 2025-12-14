const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

slot: ?usize,
name: []const u8,

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    _ = index;
    _ = ctx;
    // Associate the variable name with the slot
    if (self.slot) |slot_idx| {
        if (slot_idx < tracked.len) {
            // Store name in the slot's undefined metadata if it exists
            if (tracked[slot_idx].undefined) |*undef| {
                switch (undef.*) {
                    .undefined => |*meta| {
                        meta.var_name = self.name;
                    },
                    .defined => {},
                }
            }
        }
    }
    // Note: splat not called as no analysis needs this currently
}
