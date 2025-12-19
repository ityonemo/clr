const std = @import("std");
const tag = @import("tag.zig");
const Allocator = std.mem.Allocator;

// Import state types from analyses
const undefined_analysis = @import("analysis/undefined.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");

/// Analyte holds the analysis state for an immediate value.
/// Each analysis contributes its state type here.
pub const Analyte = struct {
    undefined: ?undefined_analysis.Undefined = null,
    memory_safety: ?memory_safety_analysis.MemorySafety = null,
};

pub const EntityList = std.array_list.AlignedManaged(TypedPayload, null);

pub const EIdx = u32;

/// TypedPayload tracks the type structure of a value along with analysis state.
/// EIdx is used for anything that needs indirection (pointers, optionals, etc).
pub const TypedPayload = union(enum) {
    immediate: Analyte,
    pointer: EIdx,
    optional: EIdx,
    @"struct": void, // temporary. Will be a slice of EIdx.
    @"union": void, // temporary. Will be a slice EIdx.

    /// Allocate a new immediate entity in the list, return its index.
    pub fn new(entities: *EntityList) EIdx {
        const idx: EIdx = @intCast(entities.items.len);
        entities.append(.{ .immediate = .{} }) catch @panic("out of memory");
        return idx;
    }
};

pub const Slot = struct {
    typed_payload: ?EIdx = null,

    pub fn apply(any_tag: tag.AnyTag, tracked: []Slot, index: usize, ctx: anytype, entities: *EntityList) !void {
        switch (any_tag) {
            inline else => |t| try t.apply(tracked, index, ctx, entities),
        }
    }

    pub fn call(called: anytype, args: anytype, tracked: []Slot, index: usize, ctx: anytype, entities: *EntityList) !void {
        _ = entities; // Each function has its own entity list, so caller's entities aren't passed
        // Skip if called is null (indirect call through function pointer - TODO: handle these)
        if (@TypeOf(called) == @TypeOf(null)) return;
        const retval = try @call(.auto, called, .{ctx} ++ args);
        // Propagate return value's analysis state to caller's slot
        tracked[index] = retval;
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

pub fn onFinish(tracked: []Slot, retval: *Slot, ctx: anytype, entities: *EntityList) !void {
    try tag.splatFinish(tracked, retval, ctx, entities);
}

test "alloc sets state to undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var entities = EntityList.init(allocator);
    defer entities.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .dbg_stmt = .{ .line = 0, .column = 0 } }, list, 0, &ctx, &entities);
    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &entities);

    // dbg_stmt has no undefined tracking
    try std.testing.expectEqual(null, list[0].typed_payload);
    // alloc marks slot as undefined
    try std.testing.expect(list[1].typed_payload != null);
    const analyte = &entities.items[list[1].typed_payload.?].immediate;
    try std.testing.expect(analyte.undefined != null);
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
    // uninitialized slot has no undefined tracking
    try std.testing.expectEqual(null, list[2].typed_payload);
}

test "store_safe with undef keeps state undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var entities = EntityList.init(allocator);
    defer entities.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &entities);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = true } }, list, 2, &ctx, &entities);

    // alloc slot stays undefined after store_safe with undef
    const analyte = &entities.items[list[1].typed_payload.?].immediate;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe with value sets state to defined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var entities = EntityList.init(allocator);
    defer entities.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &entities);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &ctx, &entities);

    // alloc slot becomes defined after store_safe with real value
    const analyte = &entities.items[list[1].typed_payload.?].immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(analyte.undefined.?));
}

// Mock context for testing load behavior
// TODO: eliminate MockContext by using real Context with suppressed output
const Meta = @import("Meta.zig");
const MockContext = struct {
    meta: Meta = .{
        .function = "test_func",
        .file = "test",
        .line = 0,
        .column = 0,
    },
    // Legacy fields
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
    var entities = EntityList.init(allocator);
    defer entities.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Set up: alloc creates undefined slot
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx, &entities);

    // Load from undefined slot should return error
    try std.testing.expectError(error.UseBeforeAssign, Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 2, &mock_ctx, &entities));
}

test "load from defined slot does not report error" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};
    var entities = EntityList.init(allocator);
    defer entities.deinit();

    const list = make_list(allocator, 4);
    defer clear_list(list, allocator);

    // Set up: alloc then store a real value
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx, &entities);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &mock_ctx, &entities);

    // Load from defined slot should NOT return error
    try Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 3, &mock_ctx, &entities);
}

// TODO: Interprocedural tests disabled during entity system refactoring.
// These tests require caller/callee slots to share entity lists, which
// needs a redesigned interprocedural analysis approach.
