const std = @import("std");
const tag = @import("tag.zig");
const Context = @import("Context.zig");
const Allocator = std.mem.Allocator;

// Import state types from analyses
const undefined_analysis = @import("analysis/undefined.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");

/// Analyte holds the analysis state for a scalar value.
/// Each analysis contributes its state type here.
pub const Analyte = struct {
    undefined: ?undefined_analysis.Undefined = null,
    memory_safety: ?memory_safety_analysis.MemorySafety = null,
};

pub const Payloads = struct {
    list: std.array_list.AlignedManaged(TypedPayload, null),

    pub fn init(allocator: Allocator) Payloads {
        return .{ .list = std.array_list.AlignedManaged(TypedPayload, null).init(allocator) };
    }

    pub fn deinit(self: *@This()) void {
        self.list.deinit();
    }

    /// Get TypedPayload by index. Crashes if index is out of bounds.
    pub fn at(self: *@This(), index: EIdx) *TypedPayload {
        return &self.list.items[index];
    }

    /// Initialize a slot with a new scalar payload. Crashes if slot already initialized.
    // TODO: Make initSlot generic, not just scalar.
    pub fn initSlot(self: *@This(), tracked: []Slot, index: usize) !EIdx {
        if (tracked[index].typed_payload) |_| @panic("slot already initialized");
        const idx: EIdx = @intCast(self.list.items.len);
        try self.list.append(.{ .scalar = .{} });
        tracked[index].typed_payload = idx;
        return idx;
    }

    /// Overwrite a slot with a new scalar payload. Creates new entity regardless of prior state.
    /// Call this in tag handlers before splat() so analyses can set their respective fields.
    /// For conditional keep-of-previous-value semantics, use merge (not yet implemented).
    pub fn clobberSlot(self: *@This(), tracked: []Slot, index: usize, value: TypedPayload) !EIdx {
        const idx: EIdx = @intCast(self.list.items.len);
        try self.list.append(value);
        tracked[index].typed_payload = idx;
        return idx;
    }

    /// Allocate a new unset_retval entity and return its index.
    /// Used for pre-allocating return value entities in caller's payloads.
    pub fn initEntity(self: *@This()) !EIdx {
        const idx: EIdx = @intCast(self.list.items.len);
        try self.list.append(.unset_retval);
        return idx;
    }

    /// Append a new entity with the given value and return its index.
    pub fn appendEntity(self: *@This(), value: TypedPayload) !EIdx {
        const idx: EIdx = @intCast(self.list.items.len);
        try self.list.append(value);
        return idx;
    }

    /// Recursively copy a TypedPayload from another Payloads list into an existing slot in this one.
    /// Handles EIdx references by recursively copying referenced entities.
    pub fn copyInto(self: *@This(), dest_idx: EIdx, src_payloads: *Payloads, src_idx: EIdx) !void {
        const src = src_payloads.list.items[src_idx];

        // IMPORTANT: We must compute the new value BEFORE assigning to self.list.items[dest_idx]
        // because copyEntityRecursive may append to self.list, which could reallocate the backing
        // array and invalidate any pointers/slices we're holding.
        const copied: TypedPayload = switch (src) {
            .pointer => |ind| blk: {
                const new_to = try self.copyEntityRecursive(src_payloads, ind.to);
                break :blk .{ .pointer = .{ .analyte = ind.analyte, .to = new_to } };
            },
            .optional => |ind| blk: {
                const new_to = try self.copyEntityRecursive(src_payloads, ind.to);
                break :blk .{ .optional = .{ .analyte = ind.analyte, .to = new_to } };
            },
            .region => |ind| blk: {
                const new_to = try self.copyEntityRecursive(src_payloads, ind.to);
                break :blk .{ .region = .{ .analyte = ind.analyte, .to = new_to } };
            },
            // These don't contain EIdx references, copy directly
            .scalar, .unset_retval, .@"struct", .@"union", .unimplemented, .void => src,
        };
        self.list.items[dest_idx] = copied;
    }

    /// Recursively copy a TypedPayload, appending to this list. Returns new index.
    // TODO: Make this non-recursive to avoid stack overflow with deeply nested structures.
    pub fn copyEntityRecursive(self: *@This(), src_payloads: *Payloads, src_idx: EIdx) !EIdx {
        const src = src_payloads.list.items[src_idx];
        // First recursively copy any referenced entities, then append this entity
        const copied: TypedPayload = switch (src) {
            .pointer => |ind| .{ .pointer = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_payloads, ind.to) } },
            .optional => |ind| .{ .optional = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_payloads, ind.to) } },
            .region => |ind| .{ .region = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_payloads, ind.to) } },
            .scalar, .unset_retval, .@"struct", .@"union", .unimplemented, .void => src,
        };
        // Compute idx AFTER recursive calls, since they may have appended entities
        const idx: EIdx = @intCast(self.list.items.len);
        try self.list.append(copied);
        return idx;
    }

    /// Assert all payloads are valid (non-null typed_payload for all tracked slots that have one).
    pub fn assertAllValid(self: *@This(), tracked: []const Slot) void {
        for (tracked, 0..) |slot, i| {
            if (slot.typed_payload) |idx| {
                if (idx >= self.list.items.len) {
                    std.debug.panic("slot {} has invalid typed_payload index {}", .{ i, idx });
                }
            }
        }
    }
};

pub const EIdx = u32;

/// TypedPayload tracks the type structure of a value along with analysis state.
/// EIdx is used for anything that needs indirection (pointers, optionals, etc).
pub const TypedPayload = union(enum) {
    const Indirected = struct {
        analyte: Analyte,
        to: EIdx,
    };

    scalar: Analyte,
    pointer: Indirected,
    optional: Indirected,
    unset_retval: void, // special-case for retval slots before a return has been called.
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": void, // unused, for now, temporarily void. Will be a slice of EIdx.
    @"union": void, // unused, for now, temporarily void. Will be a slice EIdx.
    unimplemented: void, // this means we have an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.
};

pub const Slot = struct {
    typed_payload: ?EIdx = null,

    /// Get this slot's TypedPayload. Crashes if slot has no payload.
    pub fn payload(self: *Slot, payloads: *Payloads) *TypedPayload {
        return payloads.at(self.typed_payload.?);
    }

    pub fn apply(any_tag: tag.AnyTag, tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        switch (any_tag) {
            inline else => |t| try t.apply(tracked, index, ctx, payloads),
        }
    }

    pub fn call(called: anytype, args: anytype, tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads) !void {
        // Skip if called is null (indirect call through function pointer - TODO: handle these)
        if (@TypeOf(called) == @TypeOf(null)) return;
        // Pass caller's payloads so callee can write return value into it
        const return_eidx = try @call(.auto, called, .{ ctx, payloads } ++ args);
        // Deposit returned entity index into caller's slot
        tracked[index].typed_payload = return_eidx;
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

pub fn onFinish(tracked: []Slot, ctx: *Context, payloads: *Payloads) !void {
    try tag.splatFinish(tracked, ctx, payloads);
}

test "alloc sets state to undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .dbg_stmt = .{ .line = 0, .column = 0 } }, list, 0, &ctx, &payloads);
    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &payloads);

    // dbg_stmt sets slot to void (no analysis tracking)
    try std.testing.expect(list[0].typed_payload != null);
    try std.testing.expectEqual(.void, std.meta.activeTag(list[0].payload(&payloads).*));
    // alloc marks slot as undefined
    try std.testing.expect(list[1].typed_payload != null);
    const analyte = &list[1].payload(&payloads).scalar;
    try std.testing.expect(analyte.undefined != null);
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
    // uninitialized slot has no payload yet
    try std.testing.expectEqual(null, list[2].typed_payload);
}

test "store_safe with undef keeps state undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = true } }, list, 2, &ctx, &payloads);

    // alloc slot stays undefined after store_safe with undef
    const analyte = &list[1].payload(&payloads).scalar;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe with value sets state to defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &ctx, &payloads);

    // alloc slot becomes defined after store_safe with real value
    const analyte = &list[1].payload(&payloads).scalar;
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
    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 3);
    defer clear_list(list, allocator);

    // Set up: alloc creates undefined slot
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx, &payloads);

    // Load from undefined slot should return error
    try std.testing.expectError(error.UseBeforeAssign, Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 2, &mock_ctx, &payloads));
}

test "load from defined slot does not report error" {
    const allocator = std.testing.allocator;

    var mock_ctx = MockContext{};
    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 4);
    defer clear_list(list, allocator);

    // Set up: alloc then store a real value
    try Slot.apply(.{ .alloc = .{} }, list, 1, &mock_ctx, &payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &mock_ctx, &payloads);

    // Load from defined slot should NOT return error
    try Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 3, &mock_ctx, &payloads);
}

test "all slots get valid payloads after operations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    const list = make_list(allocator, 5);
    defer clear_list(list, allocator);

    // Apply various operations that should all set payloads
    try Slot.apply(.{ .dbg_stmt = .{ .line = 0, .column = 0 } }, list, 0, &ctx, &payloads);
    try Slot.apply(.{ .alloc = .{} }, list, 1, &ctx, &payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, list, 2, &ctx, &payloads);
    try Slot.apply(.{ .load = .{ .ptr = 1 } }, list, 3, &ctx, &payloads);
    try Slot.apply(.{ .block = .{} }, list, 4, &ctx, &payloads);

    // All slots should have valid payloads
    payloads.assertAllValid(list);
}

test "ret_safe copies scalar return value to caller_payloads" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    // Callee's payloads and tracked slots
    var callee_payloads = Payloads.init(allocator);
    defer callee_payloads.deinit();
    const callee_tracked = make_list(allocator, 3);
    defer clear_list(callee_tracked, allocator);

    // Caller's payloads - pre-allocate return entity
    var caller_payloads = Payloads.init(allocator);
    defer caller_payloads.deinit();
    const return_eidx = try caller_payloads.initEntity();

    // Verify return entity is initially unset
    try std.testing.expectEqual(.unset_retval, std.meta.activeTag(caller_payloads.at(return_eidx).*));

    // In callee: allocate and store a value
    try Slot.apply(.{ .alloc = .{} }, callee_tracked, 0, &ctx, &callee_payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, callee_tracked, 1, &ctx, &callee_payloads);

    // Return the value from slot 0
    try Slot.apply(.{ .ret_safe = .{
        .caller_payloads = &caller_payloads,
        .return_eidx = return_eidx,
        .src = 0,
    } }, callee_tracked, 2, &ctx, &callee_payloads);

    // Verify return entity in caller's payloads is now a scalar
    try std.testing.expectEqual(.scalar, std.meta.activeTag(caller_payloads.at(return_eidx).*));
}

test "ret_safe with null src sets caller return to void" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    // Callee's payloads and tracked slots
    var callee_payloads = Payloads.init(allocator);
    defer callee_payloads.deinit();
    const callee_tracked = make_list(allocator, 1);
    defer clear_list(callee_tracked, allocator);

    // Caller's payloads - pre-allocate return entity
    var caller_payloads = Payloads.init(allocator);
    defer caller_payloads.deinit();
    const return_eidx = try caller_payloads.initEntity();

    // Return void (src = null)
    try Slot.apply(.{ .ret_safe = .{
        .caller_payloads = &caller_payloads,
        .return_eidx = return_eidx,
        .src = null,
    } }, callee_tracked, 0, &ctx, &callee_payloads);

    // Verify return entity in caller's payloads is now void
    try std.testing.expectEqual(.void, std.meta.activeTag(caller_payloads.at(return_eidx).*));
}

test "ret_safe with null caller_payloads (entrypoint) succeeds" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();
    const tracked = make_list(allocator, 2);
    defer clear_list(tracked, allocator);

    // Allocate a value
    try Slot.apply(.{ .alloc = .{} }, tracked, 0, &ctx, &payloads);
    try Slot.apply(.{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, tracked, 1, &ctx, &payloads);

    // Return with null caller_payloads (entrypoint case) - should just succeed without error
    try Slot.apply(.{
        .ret_safe = .{
            .caller_payloads = null,
            .return_eidx = 0, // doesn't matter when caller_payloads is null
            .src = 0,
        },
    }, tracked, 1, &ctx, &payloads);
}
