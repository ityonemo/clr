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

/// TypedPayload tracks the type structure of a value along with analysis state.
/// For pointers, it recursively contains the pointee's TypedPayload.
// TODO: We will have to turn this into an ECS-type framework
pub const TypedPayload = union(enum) {
    immediate: Analyte,
    pointer: *TypedPayload,
    @"struct": void, // temporary. Will be a slice.
    @"union": void, // temporary. Will be a slice.

    pub fn deinit(self: TypedPayload, allocator: Allocator) void {
        switch (self) {
            .pointer => |p| {
                p.deinit(allocator);
                allocator.destroy(p);
            },
            // TODO: implement "@struct" and "@union"
            else => {},
        }
    }

    /// Create a new pointer TypedPayload pointing to the given payload.
    pub fn createPointer(allocator: Allocator, pointee: TypedPayload) !*TypedPayload {
        const p = try allocator.create(TypedPayload);
        p.* = pointee;
        return p;
    }
};

pub const Slot = struct {
    typed_payload: ?TypedPayload = null,
    reference_arg: ?usize = null,
    arg_ptr: ?*Slot = null,

    pub fn apply(any_tag: tag.AnyTag, tracked: []Slot, index: usize, ctx: anytype) !void {
        switch (any_tag) {
            inline else => |t| try t.apply(tracked, index, ctx),
        }
    }

    pub fn call(called: anytype, args: anytype, tracked: []Slot, index: usize, ctx: anytype) !void {
        // Skip if called is null (indirect call through function pointer - TODO: handle these)
        if (@TypeOf(called) == @TypeOf(null)) return;
        const retval = try @call(.auto, called, .{ctx} ++ args);
        // Propagate return value's analysis state to caller's slot
        tracked[index] = retval;
    }

    /// Helper to ensure typed_payload exists with an immediate Analyte.
    /// Creates it if null. For pointers, follows the chain to the pointee.
    pub fn ensureImmediate(self: *Slot) *Analyte {
        if (self.typed_payload == null) {
            self.typed_payload = .{ .immediate = .{} };
        }
        var tp = &self.typed_payload.?;
        while (true) {
            switch (tp.*) {
                .immediate => |*a| return a,
                .pointer => |p| tp = p,
                else => unreachable,
            }
        }
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

pub fn onFinish(tracked: []Slot, retval: *Slot, ctx: anytype) !void {
    try tag.splatFinish(tracked, retval, ctx);
}

test "alloc sets state to undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .dbg_stmt = .{ .line = 0, .column = 0 } }, list, 0, &ctx);
    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);

    // dbg_stmt has no undefined tracking
    try std.testing.expectEqual(null, list[0].typed_payload);
    // alloc marks slot as undefined
    try std.testing.expect(list[1].typed_payload != null);
    const analyte = &list[1].typed_payload.?.immediate;
    try std.testing.expect(analyte.undefined != null);
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
    // uninitialized slot has no undefined tracking
    try std.testing.expectEqual(null, list[2].typed_payload);

    // reference_arg should remain null for non-arg operations
    try std.testing.expectEqual(null, list[0].reference_arg);
    try std.testing.expectEqual(null, list[1].reference_arg);
}

test "store_safe with undef keeps state undefined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = true } }, list, 2, &ctx);

    // alloc slot stays undefined after store_safe with undef
    const analyte = &list[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe with value sets state to defined" {
    const Context = @import("Context.zig");
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &ctx);

    // alloc slot becomes defined after store_safe with real value
    const analyte = &list[1].typed_payload.?.immediate;
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
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &mock_ctx);

    // Load from defined slot should NOT return error
    try Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 3, &mock_ctx);
}

test "arg sets reference_arg and arg_ptr" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Create a caller's slot that will be passed as an argument
    var caller_slot = Slot{ .typed_payload = .{ .immediate = .{ .undefined = .{ .defined = {} } } } };
    try Slot.apply(.{ .arg = .{ .value = &caller_slot, .name = "test_param" } }, list, 1, &mock_ctx);

    // Arg should set reference_arg and arg_ptr
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
    var caller_slot = Slot{ .typed_payload = .{ .immediate = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test",
        .line = 1,
    } } } } } };
    try Slot.apply(.{ .arg = .{ .value = &caller_slot, .name = "test_param" } }, list, 0, &mock_ctx);

    // Callee allocates a slot that points to the arg
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx);

    // Callee stores a defined value to the arg slot
    try Slot.apply(.{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, list, 2, &mock_ctx);

    // Both the local slot and caller's slot should now be defined
    const local_analyte = &list[0].typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(local_analyte.undefined.?));
    const caller_analyte = &caller_slot.typed_payload.?.immediate;
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_analyte.undefined.?));
}
