const std = @import("std");

pub const Slot = struct {
    state: ?State = null,
    meta: Meta = .{},

    pub const State = enum {
        undefined,
        defined,
        unknown,
    };

    pub const Meta = struct {
        file: ?[]const u8 = null,
        line: ?u32 = null,
        column: ?u32 = null,
        var_name: ?[]const u8 = null,
    };

    pub fn init(allocator: std.mem.Allocator, count: usize) []Slot {
        const slots = allocator.alloc(Slot, count) catch @panic("out of memory");
        for (slots) |*slot| {
            slot.* = .{};
        }
        return slots;
    }

    pub fn deinit(slots: []Slot, allocator: std.mem.Allocator) void {
        allocator.free(slots);
    }

    pub fn apply(comptime tag: anytype, tracked: []Slot, args: anytype) Slot {
        _ = tracked;
        _ = args;
        _ = tag;
        // For now, return an empty slot - passes will populate state as needed
        return .{};
    }
};

test "basic slot operations" {
    const allocator = std.testing.allocator;
    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    slots[0] = Slot.apply(.dbg_stmt, slots, .{});
    slots[1] = Slot.apply(.alloc, slots, .{});

    // State is null by default - passes populate it as needed
    try std.testing.expectEqual(null, slots[0].state);
    try std.testing.expectEqual(null, slots[1].state);
    try std.testing.expectEqual(null, slots[2].state);
}
