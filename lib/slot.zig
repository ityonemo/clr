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

    pub fn apply(comptime tag: anytype, tracked: []Slot, ctx: anytype, args: anytype) !Slot {
        return switch (tag) {
            .alloc => applyAlloc(),
            .store_safe => applyStoreSafe(tracked, args),
            .load => try applyLoad(tracked, ctx, args),
            .dbg_stmt => applyDbgStmt(ctx, args),
            else => .{},
        };
    }

    fn applyAlloc() Slot {
        return .{ .state = .undefined };
    }

    fn applyStoreSafe(tracked: []Slot, args: anytype) Slot {
        const ptr = args.ptr;
        if (args.is_undef) {
            tracked[ptr].state = .undefined;
        } else {
            tracked[ptr].state = .defined;
        }
        return .{};
    }

    fn applyLoad(tracked: []Slot, ctx: anytype, args: anytype) !Slot {
        const ptr = args.ptr;
        const slot = tracked[ptr];
        if (slot.state == .undefined) {
            return ctx.reportUseBeforeAssign(slot.meta);
        }
        return .{};
    }

    fn applyDbgStmt(ctx: anytype, args: anytype) Slot {
        ctx.line = ctx.base_line + args.line + 1;
        ctx.column = args.column;
        return .{};
    }
};

test "alloc sets state to undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    slots[0] = try Slot.apply(.dbg_stmt, slots, &ctx, .{});
    slots[1] = try Slot.apply(.alloc, slots, &ctx, .{});

    // dbg_stmt has no state
    try std.testing.expectEqual(null, slots[0].state);
    // alloc marks slot as undefined
    try std.testing.expectEqual(.undefined, slots[1].state);
    // uninitialized slot has no state
    try std.testing.expectEqual(null, slots[2].state);
}

test "store_safe with undef keeps state undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    slots[1] = try Slot.apply(.alloc, slots, &ctx, .{});
    slots[2] = try Slot.apply(.store_safe, slots, &ctx, .{ .ptr = 1, .is_undef = true });

    // alloc slot stays undefined after store_safe with undef
    try std.testing.expectEqual(.undefined, slots[1].state);
}

test "store_safe with value sets state to defined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    slots[1] = try Slot.apply(.alloc, slots, &ctx, .{});
    slots[2] = try Slot.apply(.store_safe, slots, &ctx, .{ .ptr = 1, .is_undef = false });

    // alloc slot becomes defined after store_safe with real value
    try std.testing.expectEqual(.defined, slots[1].state);
}

// Mock context for testing load behavior
const MockContext = struct {
    pub fn reportUseBeforeAssign(_: *MockContext, _: Slot.Meta) error{UseBeforeAssign} {
        return error.UseBeforeAssign;
    }
};

test "load from undefined slot reports use before assign" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    // Set up: alloc creates undefined slot
    slots[1] = try Slot.apply(.alloc, slots, &mock_ctx, .{});

    // Load from undefined slot should return error
    try std.testing.expectError(error.UseBeforeAssign, Slot.apply(.load, slots, &mock_ctx, .{ .ptr = 1 }));
}

test "load from defined slot does not report error" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    var slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    // Set up: alloc then store a real value
    slots[1] = try Slot.apply(.alloc, slots, &mock_ctx, .{});
    slots[2] = try Slot.apply(.store_safe, slots, &mock_ctx, .{ .ptr = 1, .is_undef = false });

    // Load from defined slot should NOT return error
    _ = try Slot.apply(.load, slots, &mock_ctx, .{ .ptr = 1 });
}
