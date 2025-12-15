const std = @import("std");
const tag = @import("tag.zig");
const Undefined = @import("analysis/undefined.zig").Undefined;
const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;

pub const Slot = struct {
    undefined: ?Undefined = null,
    memory_safety: ?MemorySafety = null,
    reference_arg: ?usize = null,
    arg_ptr: ?*Slot = null,

    pub fn apply(any_tag: tag.AnyTag, tracked: []Slot, index: usize, ctx: anytype) !void {
        switch (any_tag) {
            inline else => |t| try t.apply(tracked, index, ctx),
        }
    }

    pub fn call(called: anytype, args: anytype, tracked: []Slot, index: usize, ctx: anytype) !void {
        _ = tracked;
        _ = index;
        // Skip if called is null (indirect call through function pointer - TODO: handle these)
        if (@TypeOf(called) == @TypeOf(null)) return;
        _ = try @call(.auto, called, .{ctx} ++ args);
    }
};

pub fn make_list(allocator: std.mem.Allocator, count: usize) []Slot {
    const list = allocator.alloc(Slot, count) catch @panic("out of memory");
    for (list) |*slot| {
        slot.* = .{};
    }
    return list;
}

pub fn clear_list(list: []Slot, allocator: std.mem.Allocator) void {
    allocator.free(list);
}

test "alloc sets state to undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .dbg_stmt = .{ .line = 0, .column = 0 } }, list, 0, &ctx);
    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);

    // dbg_stmt has no undefined tracking
    try std.testing.expectEqual(null, list[0].undefined);
    // alloc marks slot as undefined
    try std.testing.expect(list[1].undefined != null);
    try std.testing.expectEqual(.undefined, std.meta.activeTag(list[1].undefined.?));
    // uninitialized slot has no undefined tracking
    try std.testing.expectEqual(null, list[2].undefined);

    // reference_arg should remain null for non-arg operations
    try std.testing.expectEqual(null, list[0].reference_arg);
    try std.testing.expectEqual(null, list[1].reference_arg);
}

test "store_safe with undef keeps state undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .is_undef = true } }, list, 2, &ctx);

    // alloc slot stays undefined after store_safe with undef
    try std.testing.expectEqual(.undefined, std.meta.activeTag(list[1].undefined.?));
}

test "store_safe with value sets state to defined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var ctx = Context.init(allocator);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .is_undef = false } }, list, 2, &ctx);

    // alloc slot becomes defined after store_safe with real value
    try std.testing.expectEqual(.defined, std.meta.activeTag(list[1].undefined.?));
}

// Mock context for testing load behavior
// TODO: eliminate MockContext by using real Context with suppressed output
const MockContext = struct {
    line: u32 = 0,
    column: u32 = 0,
    base_line: u32 = 0,
    file: []const u8 = "test",
    stacktrace: struct {
        items: []const []const u8 = &.{"test_func"},
    } = .{},

    pub fn print(_: *MockContext, comptime _: []const u8, _: anytype) void {}
};

test "load from undefined slot reports use before assign" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Set up: alloc creates undefined slot
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx);

    // Load from undefined slot should return error
    try std.testing.expectError(error.UseBeforeAssign, Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 2, &mock_ctx));
}

test "load from defined slot does not report error" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const list = make_list(allocator, 4);
    defer clear_list(list, allocator);

    // Set up: alloc then store a real value
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .is_undef = false } }, list, 2, &mock_ctx);

    // Load from defined slot should NOT return error
    try Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 3, &mock_ctx);
}

test "arg sets reference_arg and arg_ptr" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Create a caller's slot that will be passed as an argument
    var caller_slot = Slot{ .undefined = .{ .defined = {} } };
    try Slot.apply(.{ .arg = .{ .value = &caller_slot } }, list, 1, &mock_ctx);

    // Arg should copy the slot value and set reference_arg and arg_ptr
    try std.testing.expectEqual(.defined, std.meta.activeTag(list[1].undefined.?));
    try std.testing.expectEqual(@as(?usize, 1), list[1].reference_arg);
    try std.testing.expectEqual(@as(?*Slot, &caller_slot), list[1].arg_ptr);

    // Other slots should remain unaffected
    try std.testing.expectEqual(null, list[0].reference_arg);
    try std.testing.expectEqual(null, list[2].reference_arg);
}

test "store_safe propagates defined status through arg_ptr" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Simulate caller's undefined slot passed as argument
    var caller_slot = Slot{ .undefined = .{ .undefined = .{} } };
    try Slot.apply(.{ .arg = .{ .value = &caller_slot } }, list, 0, &mock_ctx);

    // Callee allocates a slot that points to the arg
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx);

    // Callee stores a defined value to the arg slot
    try Slot.apply(.{ .store_safe = .{ .ptr = 0, .is_undef = false } }, list, 2, &mock_ctx);

    // Both the local slot and caller's slot should now be defined
    try std.testing.expectEqual(.defined, std.meta.activeTag(list[0].undefined.?));
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_slot.undefined.?));
}
