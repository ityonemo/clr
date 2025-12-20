const std = @import("std");
const slots = @import("../slots.zig");
const Slot = slots.Slot;
const Payloads = slots.Payloads;
const EIdx = slots.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");

// =========================================================================
// State types
// =========================================================================

pub const StackPtr = struct {
    meta: Meta,
    name: Name = .{ .other = {} },

    pub const Name = union(enum) {
        variable: []const u8,
        parameter: []const u8,
        other: void,
    };
};

pub const Allocation = struct {
    allocated: Meta,
    freed: ?Meta = null, // null = still allocated, has value = freed
    origin: usize, // The original slot where this allocation was created
    allocator_type: []const u8, // Type of allocator that created this allocation
};

pub const MemorySafety = union(enum) {
    stack_ptr: StackPtr,
    allocation: Allocation,

    pub fn alloc(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Alloc) !void {
        _ = params;
        // Slot contains .pointer = pointee_idx, get the pointee
        const ptr_idx = tracked[index].typed_payload.?;
        const pointee_idx = payloads.at(ptr_idx).pointer;
        payloads.at(pointee_idx).scalar.memory_safety = .{ .stack_ptr = .{ .meta = ctx.meta } };
    }

    pub fn arg(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Arg) !void {
        // If the caller passed an allocation, preserve that metadata
        // so the callee can free it (ownership transfer)
        if (tracked[index].typed_payload) |idx| {
            switch (payloads.at(idx).*) {
                .scalar => |imm| {
                    if (imm.memory_safety) |ms| {
                        if (ms == .allocation) return;
                    }
                },
                .unimplemented => {},
                else => @panic("unexpected payload type in arg"),
            }
        }

        // Store parameter info with empty function name - this means returning it directly
        // won't be flagged as an escape (function won't match in ret_safe)
        _ = try payloads.clobberSlot(tracked, index, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "", // Empty = not from this function's stack
                .file = ctx.meta.file,
                .line = ctx.base_line,
            },
            .name = .{ .parameter = params.name },
        } } } });
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        // Set the variable name on the stack_ptr metadata
        const slot = params.slot orelse return;
        std.debug.assert(slot < tracked.len);
        const ptr_idx = tracked[slot].typed_payload orelse return;
        // Follow pointer to get to pointee
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .scalar => ptr_idx, // For non-pointer types, use directly
            .unimplemented => return,
            else => @panic("unexpected payload type in dbg_var_ptr (outer)"),
        };
        switch (payloads.at(pointee_idx).*) {
            .scalar => |*imm| {
                const ms = &(imm.memory_safety orelse return);
                if (ms.* != .stack_ptr) return;
                if (ms.stack_ptr.name == .other) {
                    ms.stack_ptr.name = .{ .variable = params.name };
                }
            },
            .unimplemented => {},
            else => @panic("unexpected payload type in dbg_var_ptr (pointee)"),
        }
    }

    pub fn bitcast(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Bitcast) !void {
        _ = ctx;
        // Slot created by tag handler before splat
        const idx = tracked[index].typed_payload.?;
        const src = params.src orelse return;
        std.debug.assert(src < tracked.len);
        const src_idx = tracked[src].typed_payload orelse return;
        switch (payloads.at(src_idx).*) {
            .scalar => |imm| payloads.at(idx).scalar.memory_safety = imm.memory_safety,
            .unimplemented => {},
            else => @panic("unexpected payload type in bitcast"),
        }
    }

    pub fn unwrap_errunion_payload(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.UnwrapErrunionPayload) !void {
        // Entity structure and analysis state already copied by tag handler
        _ = tracked;
        _ = index;
        _ = ctx;
        _ = payloads;
        _ = params;
    }

    pub fn optional_payload(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.OptionalPayload) !void {
        _ = ctx;
        // Slot created by tag handler before splat
        const idx = tracked[index].typed_payload.?;
        const src = params.src orelse return;
        std.debug.assert(src < tracked.len);
        const src_idx = tracked[src].typed_payload orelse return;
        switch (payloads.at(src_idx).*) {
            .scalar => |imm| payloads.at(idx).scalar.memory_safety = imm.memory_safety,
            .unimplemented => {},
            else => @panic("unexpected payload type in optional_payload"),
        }
    }

    pub fn br(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Br) !void {
        // Entity structure and analysis state already copied by tag handler
        _ = tracked;
        _ = index;
        _ = ctx;
        _ = payloads;
        _ = params;
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.StoreSafe) !void {
        // TODO: Interprocedural parameter name propagation disabled during entity system refactoring.
        // Previously checked reference_arg to detect storing from arg slot to alloc slot.
        _ = tracked;
        _ = index;
        _ = ctx;
        _ = payloads;
        _ = params;
    }

    pub fn ret_safe(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.RetSafe) !void {
        _ = index;

        const src = params.src orelse return;
        std.debug.assert(src < tracked.len);

        const src_idx = tracked[src].typed_payload orelse return;
        const typed_payload = payloads.at(src_idx);
        if (typed_payload.* != .scalar) return;
        const ms = typed_payload.scalar.memory_safety orelse return;

        // Check for stack pointer escape
        switch (ms) {
            .stack_ptr => |sp| {
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                // Only flag as escape if stack_ptr is from this function
                // Args have empty function name, so they won't match
                if (std.mem.eql(u8, sp.meta.function, func_name)) {
                    return reportStackEscape(ms, ctx);
                }
            },
            .allocation => {},
        }

        // Note: Interprocedural propagation is now handled in RetSafe.apply in tag.zig
        // via caller_payloads and return_eidx. Memory safety state is copied along with
        // the TypedPayload when return values are propagated to the caller.
    }

    /// Called at the end of each function to check for memory leaks.
    /// Deferred until after all slots are processed so success paths can free
    /// allocations before we check for leaks.
    pub fn onFinish(tracked: []Slot, ctx: *Context, payloads: *Payloads) !void {
        // TODO: Interprocedural ownership transfer disabled during entity system refactoring
        const returned_origin: ?usize = null;

        for (tracked) |slot| {
            const idx = slot.typed_payload orelse continue;
            // Follow pointer to get to scalar
            const pointee_idx = switch (payloads.at(idx).*) {
                .pointer => |pidx| pidx,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (payloads.at(pointee_idx).*) {
                .scalar => |imm| imm,
                else => continue,
            };
            const ms = imm.memory_safety orelse continue;
            if (ms != .allocation) continue;
            const a = ms.allocation;

            // Skip if this allocation is being returned (ownership transfer)
            if (returned_origin) |origin| {
                if (a.origin == origin) continue;
            }
            if (a.freed == null) {
                // Before reporting leak, check if any slot with same origin was freed
                // (handles case where callee freed via arg_ptr propagation)
                if (isOriginFreed(tracked, payloads, a.origin)) continue;
                // Still allocated after all paths = leak
                return reportMemoryLeak(ctx, a.allocated);
            }
        }
    }

    /// Check if any slot with the given origin has been freed
    fn isOriginFreed(tracked: []Slot, payloads: *Payloads, origin: usize) bool {
        for (tracked) |slot| {
            const idx = slot.typed_payload orelse continue;
            // Follow pointer to get to scalar
            const pointee_idx = switch (payloads.at(idx).*) {
                .pointer => |pidx| pidx,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (payloads.at(pointee_idx).*) {
                .scalar => |imm| imm,
                else => continue,
            };
            const ms = imm.memory_safety orelse continue;
            if (ms != .allocation) continue;
            if (ms.allocation.origin == origin and ms.allocation.freed != null) {
                return true;
            }
        }
        return false;
    }

    // =========================================================================
    // Allocation tracking (use-after-free, double-free, memory leak detection)
    // =========================================================================

    /// Handle allocator.create() - marks pointed-to memory as allocated
    pub fn alloc_create(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.AllocCreate) !void {
        // Slot contains .pointer = pointee_idx, get the pointee
        const ptr_idx = tracked[index].typed_payload.?;
        const pointee_idx = payloads.at(ptr_idx).pointer;
        payloads.at(pointee_idx).scalar.memory_safety = .{ .allocation = .{
            .allocated = ctx.meta,
            .origin = index, // This slot is the origin of this allocation
            .allocator_type = params.allocator_type,
        } };
    }

    /// Handle allocator.destroy() - marks as freed, detects double-free and mismatched allocator
    pub fn alloc_destroy(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.AllocDestroy) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        std.debug.assert(ptr < tracked.len);

        // Follow the pointer to get the pointee
        const ptr_idx = tracked[ptr].typed_payload orelse @panic("alloc_destroy: slot has no typed_payload");
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .unimplemented => return,
            else => @panic("alloc_destroy: expected pointer type"),
        };
        const imm = switch (payloads.at(pointee_idx).*) {
            .scalar => |imm| imm,
            .unimplemented => return,
            else => @panic("alloc_destroy: pointee is not scalar"),
        };
        const ms = imm.memory_safety orelse @panic("alloc_destroy: pointee has no memory_safety");

        switch (ms) {
            .stack_ptr => |sp| return reportFreeStackMemory(ctx, sp),
            .allocation => |a| {
                if (a.freed) |previous_free| {
                    return reportDoubleFree(ctx, a.allocated, previous_free);
                }
                // Check for mismatched allocator types
                const alloc_is_generic = std.mem.eql(u8, a.allocator_type, "Allocator");
                const free_is_generic = std.mem.eql(u8, params.allocator_type, "Allocator");
                // TODO: Interprocedural - previously used arg_ptr to detect came_from_param
                const came_from_param = false;
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                const came_from_other_func = !std.mem.eql(u8, a.allocated.function, func_name);

                const should_check = if (came_from_param or came_from_other_func)
                    !alloc_is_generic and !free_is_generic
                else
                    !alloc_is_generic or !free_is_generic;

                if (should_check and !std.mem.eql(u8, a.allocator_type, params.allocator_type)) {
                    return reportMismatchedAllocator(ctx, a, params.allocator_type);
                }
                markAllocationFreed(tracked, payloads, a.origin, ctx.meta);
            },
        }
    }

    /// Mark all slots with the given origin as freed.
    fn markAllocationFreed(tracked: []Slot, payloads: *Payloads, origin: usize, free_meta: Meta) void {
        for (tracked) |slot| {
            const idx = slot.typed_payload orelse continue;
            // Follow pointer to get to scalar
            const pointee_idx = switch (payloads.at(idx).*) {
                .pointer => |pidx| pidx,
                .unimplemented, .void, .scalar => continue,
                else => continue,
            };
            const imm = switch (payloads.at(pointee_idx).*) {
                .scalar => |*imm| imm,
                else => continue,
            };
            const ms = &(imm.memory_safety orelse continue);
            if (ms.* != .allocation) continue;
            if (ms.allocation.origin == origin) {
                ms.allocation.freed = free_meta;
            }
        }
    }

    /// Handle load - detect use-after-free
    pub fn load(tracked: []Slot, index: usize, ctx: *Context, payloads: *Payloads, params: tag.Load) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        std.debug.assert(ptr < tracked.len);

        // Follow the pointer to get the pointee
        const ptr_idx = tracked[ptr].typed_payload orelse return;
        const pointee_idx = switch (payloads.at(ptr_idx).*) {
            .pointer => |idx| idx,
            .unimplemented => return,
            else => return, // Not a pointer type, nothing to check
        };
        switch (payloads.at(pointee_idx).*) {
            .scalar => |imm| {
                const ms = imm.memory_safety orelse return;
                if (ms != .allocation) return;
                const a = ms.allocation;
                if (a.freed) |free_site| {
                    return reportUseAfterFree(ctx, a.allocated, free_site);
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // Error reporting
    // =========================================================================

    fn reportStackEscape(ms: MemorySafety, ctx: *Context) anyerror {
        const sp = ms.stack_ptr;
        try ctx.meta.print(ctx.writer, "stack pointer escape in ", .{});
        switch (sp.name) {
            .variable => |name| {
                try sp.meta.print(ctx.writer, "pointer was for local variable '{s}' in ", .{name});
            },
            .parameter => |name| {
                if (name.len > 0) {
                    try sp.meta.print(ctx.writer, "pointer was for parameter '{s}' created in ", .{name});
                } else {
                    try sp.meta.print(ctx.writer, "pointer was for parameter created in ", .{});
                }
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer was for stack memory created in ", .{});
            },
        }
        return error.StackPointerEscape;
    }

    fn reportDoubleFree(ctx: *Context, alloc_site: Meta, previous_free: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "double free in ", .{});
        try previous_free.print(ctx.writer, "previously freed in ", .{});
        try alloc_site.print(ctx.writer, "originally allocated in ", .{});
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: *Context, alloc_site: Meta, free_site: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "use after free in ", .{});
        try free_site.print(ctx.writer, "freed in ", .{});
        try alloc_site.print(ctx.writer, "allocated in ", .{});
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: *Context, alloc_site: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "memory leak in ", .{});
        try alloc_site.print(ctx.writer, "allocated in ", .{});
        return error.MemoryLeak;
    }

    fn reportMismatchedAllocator(ctx: *Context, allocation: Allocation, destroy_allocator: []const u8) anyerror {
        try ctx.meta.print(ctx.writer, "allocator mismatch in ", .{});
        try allocation.allocated.print(ctx.writer, "allocated with {s} in ", .{allocation.allocator_type});
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "freed with {s}\n", .{destroy_allocator}) catch return error.FormatError;
        try ctx.writer.writeAll(msg);
        return error.MismatchedAllocator;
    }

    fn reportFreeStackMemory(ctx: *Context, sp: StackPtr) anyerror {
        try ctx.meta.print(ctx.writer, "free of stack memory in ", .{});
        switch (sp.name) {
            .variable => |name| {
                try sp.meta.print(ctx.writer, "pointer is to local variable '{s}' in ", .{name});
            },
            .parameter => |name| {
                try sp.meta.print(ctx.writer, "pointer is to parameter '{s}' in ", .{name});
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer is to stack memory in ", .{});
            },
        }
        return error.FreeStackMemory;
    }
};

// =========================================================================
// Tests
// =========================================================================

// Mock context for testing
const MockContext = struct {
    meta: Meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 10,
        .column = 5,
    },
    // Legacy fields for arg handler and reporting functions
    file: []const u8 = "test.zig",
    line: u32 = 10,
    column: u32 = 5,
    base_line: u32 = 1,
    stacktrace: std.ArrayList([]const u8),
    output: std.ArrayList(u8),

    pub fn init(allocator: std.mem.Allocator) MockContext {
        var ctx = MockContext{
            .stacktrace = std.ArrayList([]const u8).init(allocator),
            .output = std.ArrayList(u8).init(allocator),
        };
        ctx.stacktrace.append("test_func") catch unreachable;
        return ctx;
    }

    pub fn deinit(self: *MockContext) void {
        self.stacktrace.deinit();
        self.output.deinit();
    }

    pub fn print(self: *MockContext, comptime fmt: []const u8, args: anytype) void {
        std.fmt.format(self.output.writer(), fmt, args) catch unreachable;
    }
};

test "alloc sets stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.alloc(&tracked, 1, &ctx, &payloads, .{});

    const ms = payloads.at(tracked[1].typed_payload.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.meta.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
}

test "arg sets stack_ptr with empty function and parameter name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.arg(&tracked, 0, &ctx, &payloads, .{ .value = undefined, .name = "my_param" });

    const ms = payloads.at(tracked[0].typed_payload.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 1), ms.stack_ptr.meta.line); // base_line
    try std.testing.expectEqual(.parameter, std.meta.activeTag(ms.stack_ptr.name));
    try std.testing.expectEqualStrings("my_param", ms.stack_ptr.name.parameter);
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First alloc to set up stack_ptr with .other name
    try MemorySafety.alloc(&tracked, 1, &ctx, &payloads, .{});
    const ms1 = payloads.at(tracked[1].typed_payload.?).scalar.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try MemorySafety.dbg_var_ptr(&tracked, 0, &ctx, &payloads, .{ .slot = 1, .name = "foo" });

    const ms2 = payloads.at(tracked[1].typed_payload.?).scalar.memory_safety.?;
    try std.testing.expectEqual(.variable, std.meta.activeTag(ms2.stack_ptr.name));
    try std.testing.expectEqualStrings("foo", ms2.stack_ptr.name.variable);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up source with stack_ptr
    _ = try payloads.clobberSlot(&tracked, 0, .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "source_func",
            .file = "source.zig",
            .line = 42,
            .column = 7,
        },
        .name = .{ .variable = "src_var" },
    } } });

    try MemorySafety.bitcast(&tracked, 1, &ctx, &payloads, .{ .src = 0 });

    const ms = payloads.at(tracked[1].typed_payload.?).scalar.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.meta.line);
    try std.testing.expectEqualStrings("src_var", ms.stack_ptr.name.variable);
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot with stack_ptr from test_func (current function)
    _ = try payloads.clobberSlot(&tracked, 0, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 5,
        },
        .name = .{ .variable = "local" },
    } } } });

    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(&tracked, 1, &ctx, &payloads, .{ .caller_payloads = null, .return_eidx = 0, .src = 0 }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var payloads = Payloads.init(allocator);
    defer payloads.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Slot with empty function name (arg)
    _ = try payloads.clobberSlot(&tracked, 0, .{ .scalar = .{ .memory_safety = .{ .stack_ptr = .{
        .meta = .{
            .function = "",
            .file = "test.zig",
            .line = 5,
        },
        .name = .{ .parameter = "param" },
    } } } });

    // Should NOT error - returning arg is fine
    try MemorySafety.ret_safe(&tracked, 1, &ctx, &payloads, .{ .caller_payloads = null, .return_eidx = 0, .src = 0 });
}
