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

    pub fn apply(comptime tag: anytype, tracked: []Slot, index: usize, ctx: anytype, args: anytype) !void {
        switch (tag) {
            .alloc => applyAlloc(tracked, index),
            .arg => applyArg(tracked, index, args),
            .store_safe => applyStoreSafe(tracked, args),
            .load => try applyLoad(tracked, ctx, args),
            .dbg_stmt => applyDbgStmt(ctx, args),
            .ret_safe => applyRetSafe(tracked, args),
            .call, .call_always_tail, .call_never_tail, .call_never_inline => try applyCall(args),
            else => {},
        }
    }

    fn applyAlloc(tracked: []Slot, index: usize) void {
        tracked[index] = .{ .state = .undefined };
    }

    fn applyArg(tracked: []Slot, index: usize, args: anytype) void {
        tracked[index] = args[0];
    }

    fn applyStoreSafe(tracked: []Slot, args: anytype) void {
        // Skip if ptr is null (global/interned pointer - TODO: handle these)
        if (@TypeOf(args.ptr) == @TypeOf(null)) return;
        const ptr = args.ptr;
        if (args.is_undef) {
            tracked[ptr].state = .undefined;
        } else {
            tracked[ptr].state = .defined;
        }
    }

    fn applyLoad(tracked: []Slot, ctx: anytype, args: anytype) !void {
        // Skip if ptr is null (global/interned pointer - TODO: handle these)
        if (@TypeOf(args.ptr) == @TypeOf(null)) return;
        const ptr = args.ptr;
        const slot = tracked[ptr];
        if (slot.state == .undefined) {
            return ctx.reportUseBeforeAssign(slot.meta);
        }
    }

    fn applyDbgStmt(ctx: anytype, args: anytype) void {
        ctx.line = ctx.base_line + args.line + 1;
        ctx.column = args.column;
    }

    fn applyRetSafe(tracked: []Slot, args: anytype) void {
        if (@hasField(@TypeOf(args), "src")) {
            args.retval_ptr.* = tracked[args.src];
        }
    }

    fn applyCall(args: anytype) !void {
        // Skip if called is null (indirect call through function pointer - TODO: handle these)
        if (@TypeOf(args.called) == @TypeOf(null)) return;
        _ = try @call(.auto, args.called, args.args);
    }
};

test "alloc sets state to undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    const slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    try Slot.apply(.dbg_stmt, slots, 0, &ctx, .{ .line = 0, .column = 0 });
    try Slot.apply(.alloc, slots, 1, &ctx, .{});

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

    const slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    try Slot.apply(.alloc, slots, 1, &ctx, .{});
    try Slot.apply(.store_safe, slots, 2, &ctx, .{ .ptr = 1, .is_undef = true });

    // alloc slot stays undefined after store_safe with undef
    try std.testing.expectEqual(.undefined, slots[1].state);
}

test "store_safe with value sets state to defined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    const slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    try Slot.apply(.alloc, slots, 1, &ctx, .{});
    try Slot.apply(.store_safe, slots, 2, &ctx, .{ .ptr = 1, .is_undef = false });

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

    const slots = Slot.init(allocator, 3);
    defer Slot.deinit(slots, allocator);

    // Set up: alloc creates undefined slot
    try Slot.apply(.alloc, slots, 1, &mock_ctx, .{});

    // Load from undefined slot should return error
    try std.testing.expectError(error.UseBeforeAssign, Slot.apply(.load, slots, 2, &mock_ctx, .{ .ptr = 1 }));
}

test "load from defined slot does not report error" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const slots = Slot.init(allocator, 4);
    defer Slot.deinit(slots, allocator);

    // Set up: alloc then store a real value
    try Slot.apply(.alloc, slots, 1, &mock_ctx, .{});
    try Slot.apply(.store_safe, slots, 2, &mock_ctx, .{ .ptr = 1, .is_undef = false });

    // Load from defined slot should NOT return error
    try Slot.apply(.load, slots, 3, &mock_ctx, .{ .ptr = 1 });
}
