
const std = @import("std");
const Slot = @import("../slots.zig").Slot;

// =========================================================================
// State types
// =========================================================================

pub const StackPtrMeta = struct {
    function: []const u8,
    file: []const u8,
    line: u32,
    column: ?u32 = null,
    name: Name = .{ .other = {} },

    pub const Name = union(enum) {
        variable: []const u8,
        parameter: []const u8,
        other: void,
    };
};

pub const Meta = struct {
    file: []const u8,
    line: u32,
    column: ?u32 = null,
    function: []const u8,
};

pub const Allocation = struct {
    allocated: Meta,
    freed: ?Meta = null, // null = still allocated, has value = freed
    origin: usize, // The original slot where this allocation was created
    allocator_type: []const u8, // Type of allocator that created this allocation
};

/// State is the analysis state stored in Analyte.memory_safety
pub const State = union(enum) {
    stack_ptr: StackPtrMeta,
    allocation: Allocation,
};

// =========================================================================
// Analysis handlers
// =========================================================================

pub const MemorySafety = struct {
    pub fn alloc(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = payload;
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        const analyte = tracked[index].ensureImmediate();
        analyte.memory_safety = .{ .stack_ptr = .{
            .function = func_name,
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
        } };
    }

    pub fn arg(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        // If the caller passed an allocation, preserve that metadata
        // so the callee can free it (ownership transfer)
        if (tracked[index].typed_payload) |tp| {
            if (tp.immediate.memory_safety) |ms| {
                if (ms == .allocation) return;
            }
        }

        // Store parameter info with empty function name - this means returning it directly
        // won't be flagged as an escape (function won't match in ret_safe)
        const analyte = tracked[index].ensureImmediate();
        analyte.memory_safety = .{ .stack_ptr = .{
            .function = "", // Empty = not from this function's stack
            .file = ctx.file,
            .line = ctx.base_line,
            .name = .{ .parameter = payload.name },
        } };
    }

    pub fn dbg_var_ptr(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        // Set the variable name on the stack_ptr metadata
        const slot = payload.slot orelse return;
        std.debug.assert(slot < tracked.len);
        const tp = &(tracked[slot].typed_payload orelse return);
        const ms = tp.immediate.memory_safety orelse return;
        if (ms != .stack_ptr) return;
        if (ms.stack_ptr.name == .other) {
            tp.immediate.memory_safety.?.stack_ptr.name = .{ .variable = payload.name };
        }
    }

    pub fn bitcast(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        // Bitcast just reinterprets the pointer type (e.g., *u8 -> *const u8)
        // Propagate memory_safety metadata from source to result
        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);
        const src_tp = tracked[src].typed_payload orelse return;
        const ms = src_tp.immediate.memory_safety orelse return;
        const dst_analyte = tracked[index].ensureImmediate();
        dst_analyte.memory_safety = ms;
    }

    pub fn unwrap_errunion_payload(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        // Unwrapping an error union extracts the payload value
        // Propagate memory_safety metadata from source to result
        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);
        const src_tp = tracked[src].typed_payload orelse return;
        const ms = src_tp.immediate.memory_safety orelse return;
        const dst_analyte = tracked[index].ensureImmediate();
        dst_analyte.memory_safety = ms;
    }

    pub fn optional_payload(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = ctx;
        // Unwrapping an optional extracts the payload value
        // Propagate memory_safety metadata from source to result
        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);
        const src_tp = tracked[src].typed_payload orelse return;
        const ms = src_tp.immediate.memory_safety orelse return;
        const dst_analyte = tracked[index].ensureImmediate();
        dst_analyte.memory_safety = ms;
    }

    pub fn br(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        // Branch to block with value - propagate state from src to target block
        const src = payload.src orelse return;
        const block = payload.block;
        std.debug.assert(src < tracked.len);
        std.debug.assert(block < tracked.len);
        const src_tp = tracked[src].typed_payload orelse return;
        const ms = src_tp.immediate.memory_safety orelse return;
        const dst_analyte = tracked[block].ensureImmediate();
        dst_analyte.memory_safety = ms;
    }

    pub fn store_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        _ = ctx;
        const ptr = payload.ptr orelse return;
        const src = payload.src orelse return;
        std.debug.assert(ptr < tracked.len);
        std.debug.assert(src < tracked.len);

        // Check if storing from an arg slot to an alloc slot (parameter case)
        // Transfer the arg's param name/line to the alloc's stack_ptr
        if (tracked[src].reference_arg == null) return;
        const src_tp = tracked[src].typed_payload orelse return;
        const src_ms = src_tp.immediate.memory_safety orelse return;
        const dst_tp = &(tracked[ptr].typed_payload orelse return);
        if (dst_tp.immediate.memory_safety == null) return;
        if (dst_tp.immediate.memory_safety.? != .stack_ptr) return;
        // Keep dst's function (the current function) so it's detected as escape
        dst_tp.immediate.memory_safety.?.stack_ptr.name = src_ms.stack_ptr.name;
        dst_tp.immediate.memory_safety.?.stack_ptr.line = src_ms.stack_ptr.line;
        dst_tp.immediate.memory_safety.?.stack_ptr.column = null;
    }

    pub fn ret_safe(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;

        const src = payload.src orelse return;
        std.debug.assert(src < tracked.len);

        const tp = tracked[src].typed_payload orelse return;
        const ms = tp.immediate.memory_safety orelse return;

        // Check for stack pointer escape
        switch (ms) {
            .stack_ptr => |sp| {
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                // Only flag as escape if stack_ptr is from this function
                // Args have empty function name, so they won't match
                if (std.mem.eql(u8, sp.function, func_name)) {
                    return reportStackEscape(ms, ctx);
                }
            },
            .allocation => {},
        }

        // Merge state into retval for ownership transfer tracking.
        // If we're returning an allocation, it's not a leak - ownership transfers to caller.
        const retval_analyte = payload.retval_ptr.ensureImmediate();
        retval_analyte.memory_safety = ms;
    }

    /// Called at the end of each function to check for memory leaks.
    /// Deferred until after all slots are processed so success paths can free
    /// allocations before we check for leaks.
    pub fn onFinish(tracked: []Slot, retval: *Slot, ctx: anytype) !void {
        // Get the origin of any allocation being returned (ownership transfer)
        const returned_origin: ?usize = blk: {
            const retval_tp = retval.typed_payload orelse break :blk null;
            const ms = retval_tp.immediate.memory_safety orelse break :blk null;
            break :blk switch (ms) {
                .allocation => |a| a.origin,
                .stack_ptr => null,
            };
        };

        for (tracked) |slot| {
            const tp = slot.typed_payload orelse continue;
            const ms = tp.immediate.memory_safety orelse continue;
            if (ms != .allocation) continue;
            const a = ms.allocation;

            // Skip if this allocation is being returned (ownership transfer)
            if (returned_origin) |origin| {
                if (a.origin == origin) continue;
            }
            if (a.freed == null) {
                // Before reporting leak, check if any slot with same origin was freed
                // (handles case where callee freed via arg_ptr propagation)
                if (isOriginFreed(tracked, a.origin)) continue;
                // Still allocated after all paths = leak
                return reportMemoryLeak(ctx, a.allocated);
            }
        }
    }

    /// Check if any slot with the given origin has been freed
    fn isOriginFreed(tracked: []Slot, origin: usize) bool {
        for (tracked) |slot| {
            const tp = slot.typed_payload orelse continue;
            const ms = tp.immediate.memory_safety orelse continue;
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

    fn makeMeta(ctx: anytype) Meta {
        return .{
            .file = ctx.file,
            .line = ctx.line,
            .column = ctx.column,
            .function = ctx.stacktrace.items[ctx.stacktrace.items.len - 1],
        };
    }

    /// Handle allocator.create() - marks slot as allocated
    pub fn alloc_create(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        const analyte = tracked[index].ensureImmediate();
        analyte.memory_safety = .{ .allocation = .{
            .allocated = makeMeta(ctx),
            .origin = index, // This slot is the origin of this allocation
            .allocator_type = payload.allocator_type,
        } };
    }

    /// Handle allocator.destroy() - marks as freed, detects double-free and mismatched allocator
    pub fn alloc_destroy(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        std.debug.assert(ptr < tracked.len);

        const free_meta = makeMeta(ctx);

        const tp = &(tracked[ptr].typed_payload orelse {
            // Slot wasn't tracked as allocated (allocation state didn't propagate)
            // Mark it as freed anyway so we can detect use-after-free
            const new_analyte = tracked[ptr].ensureImmediate();
            new_analyte.memory_safety = .{ .allocation = .{
                .allocated = free_meta, // Use destroy site as placeholder
                .freed = free_meta,
                .origin = ptr,
                .allocator_type = payload.allocator_type,
            } };
            return;
        });

        const ms = tp.immediate.memory_safety orelse {
            tp.immediate.memory_safety = .{ .allocation = .{
                .allocated = free_meta,
                .freed = free_meta,
                .origin = ptr,
                .allocator_type = payload.allocator_type,
            } };
            return;
        };

        switch (ms) {
            .stack_ptr => |sp| return reportFreeStackMemory(ctx, sp),
            .allocation => |a| {
                if (a.freed) |previous_free| {
                    return reportDoubleFree(ctx, a.allocated, previous_free);
                }
                // Check for mismatched allocator types
                const alloc_is_generic = std.mem.eql(u8, a.allocator_type, "Allocator");
                const free_is_generic = std.mem.eql(u8, payload.allocator_type, "Allocator");
                const came_from_param = tracked[ptr].arg_ptr != null;
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                const came_from_other_func = !std.mem.eql(u8, a.allocated.function, func_name);

                const should_check = if (came_from_param or came_from_other_func)
                    !alloc_is_generic and !free_is_generic
                else
                    !alloc_is_generic or !free_is_generic;

                if (should_check and !std.mem.eql(u8, a.allocator_type, payload.allocator_type)) {
                    return reportMismatchedAllocator(ctx, a, payload.allocator_type);
                }
                markAllocationFreed(tracked, a.origin, free_meta);

                // Propagate freed state back to caller via arg_ptr
                if (tracked[ptr].arg_ptr) |caller_slot| {
                    if (caller_slot.typed_payload) |*caller_tp| {
                        if (caller_tp.immediate.memory_safety) |*caller_ms| {
                            if (caller_ms.* == .allocation) {
                                caller_ms.allocation.freed = free_meta;
                            }
                        }
                    }
                }
            },
        }
    }

    /// Mark all slots with the given origin as freed.
    fn markAllocationFreed(tracked: []Slot, origin: usize, free_meta: Meta) void {
        for (tracked) |*slot| {
            const tp = &(slot.typed_payload orelse continue);
            const ms = tp.immediate.memory_safety orelse continue;
            if (ms != .allocation) continue;
            if (ms.allocation.origin == origin) {
                tp.immediate.memory_safety.?.allocation.freed = free_meta;
            }
        }
    }

    /// Handle load - detect use-after-free
    pub fn load(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
        _ = index;
        const ptr = payload.ptr orelse return;
        std.debug.assert(ptr < tracked.len);

        const tp = tracked[ptr].typed_payload orelse return;
        const ms = tp.immediate.memory_safety orelse return;
        if (ms != .allocation) return;
        const a = ms.allocation;
        if (a.freed) |free_site| {
            return reportUseAfterFree(ctx, a.allocated, free_site);
        }
    }

    // =========================================================================
    // Error reporting
    // =========================================================================

    fn reportStackEscape(ms: State, ctx: anytype) error{StackPointerEscape} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        const meta = ms.stack_ptr;
        ctx.print("stack pointer escape in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        switch (meta.name) {
            .variable => |name| {
                ctx.print("pointer was for local variable '{s}' ({s}:{d}:{d})\n", .{
                    name,
                    meta.file,
                    meta.line,
                    meta.column orelse 0,
                });
            },
            .parameter => |name| {
                if (name.len > 0) {
                    ctx.print("pointer was for parameter '{s}' created in {s} ({s}:{d})\n", .{
                        name,
                        meta.function,
                        meta.file,
                        meta.line + 1, // Convert 0-indexed to 1-indexed
                    });
                } else {
                    ctx.print("pointer was for parameter created in {s} ({s}:{d})\n", .{
                        meta.function,
                        meta.file,
                        meta.line + 1, // Convert 0-indexed to 1-indexed
                    });
                }
            },
            .other => {
                ctx.print("pointer was for stack memory created in {s} ({s}:{d}:{d})\n", .{
                    meta.function,
                    meta.file,
                    meta.line,
                    meta.column orelse 0,
                });
            },
        }
        return error.StackPointerEscape;
    }

    fn reportDoubleFree(ctx: anytype, alloc_site: Meta, previous_free: Meta) error{DoubleFree} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("double free in {s} ({s}:{d}:{d})\n", .{
            func_name, ctx.file, ctx.line, ctx.column,
        });
        ctx.print("previously freed in {s} ({s}:{d}:{d})\n", .{
            previous_free.function, previous_free.file, previous_free.line, previous_free.column orelse 0,
        });
        ctx.print("originally allocated in {s} ({s}:{d}:{d})\n", .{
            alloc_site.function, alloc_site.file, alloc_site.line, alloc_site.column orelse 0,
        });
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: anytype, alloc_site: Meta, free_site: Meta) error{UseAfterFree} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("use after free in {s} ({s}:{d}:{d})\n", .{
            func_name, ctx.file, ctx.line, ctx.column,
        });
        ctx.print("freed in {s} ({s}:{d}:{d})\n", .{
            free_site.function, free_site.file, free_site.line, free_site.column orelse 0,
        });
        ctx.print("allocated in {s} ({s}:{d}:{d})\n", .{
            alloc_site.function, alloc_site.file, alloc_site.line, alloc_site.column orelse 0,
        });
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: anytype, alloc_site: Meta) error{MemoryLeak} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("memory leak in {s} ({s}:{d}:{d})\n", .{
            func_name, ctx.file, ctx.line, ctx.column,
        });
        ctx.print("allocated in {s} ({s}:{d}:{d})\n", .{
            alloc_site.function, alloc_site.file, alloc_site.line, alloc_site.column orelse 0,
        });
        return error.MemoryLeak;
    }

    fn reportMismatchedAllocator(ctx: anytype, allocation: Allocation, destroy_allocator: []const u8) error{MismatchedAllocator} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("allocator mismatch in {s} ({s}:{d}:{d})\n", .{
            func_name, ctx.file, ctx.line, ctx.column,
        });
        ctx.print("allocated with {s} in {s} ({s}:{d}:{d})\n", .{
            allocation.allocator_type,
            allocation.allocated.function,
            allocation.allocated.file,
            allocation.allocated.line,
            allocation.allocated.column orelse 0,
        });
        ctx.print("freed with {s}\n", .{destroy_allocator});
        return error.MismatchedAllocator;
    }

    fn reportFreeStackMemory(ctx: anytype, stack_ptr: StackPtrMeta) error{FreeStackMemory} {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("free of stack memory in {s} ({s}:{d}:{d})\n", .{
            func_name, ctx.file, ctx.line, ctx.column,
        });
        switch (stack_ptr.name) {
            .variable => |name| {
                ctx.print("pointer is to local variable '{s}' ({s}:{d}:{d})\n", .{
                    name, stack_ptr.file, stack_ptr.line, stack_ptr.column orelse 0,
                });
            },
            .parameter => |name| {
                ctx.print("pointer is to parameter '{s}' ({s}:{d})\n", .{
                    name, stack_ptr.file, stack_ptr.line + 1,
                });
            },
            .other => {
                ctx.print("pointer is to stack memory ({s}:{d}:{d})\n", .{
                    stack_ptr.file, stack_ptr.line, stack_ptr.column orelse 0,
                });
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
    line: u32 = 10,
    column: u32 = 5,
    base_line: u32 = 1,
    file: []const u8 = "test.zig",
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

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.alloc(&tracked, 1, &ctx, .{});

    const analyte = &tracked[1].typed_payload.?.immediate;
    const ms = analyte.memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
}

test "arg sets stack_ptr with empty function and parameter name" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    try MemorySafety.arg(&tracked, 0, &ctx, .{ .value = undefined, .name = "my_param" });

    const analyte = &tracked[0].typed_payload.?.immediate;
    const ms = analyte.memory_safety.?;
    try std.testing.expectEqualStrings("", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 1), ms.stack_ptr.line); // base_line
    try std.testing.expectEqual(.parameter, std.meta.activeTag(ms.stack_ptr.name));
    try std.testing.expectEqualStrings("my_param", ms.stack_ptr.name.parameter);
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // First alloc to set up stack_ptr with .other name
    try MemorySafety.alloc(&tracked, 1, &ctx, .{});
    const analyte1 = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.other, std.meta.activeTag(analyte1.memory_safety.?.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try MemorySafety.dbg_var_ptr(&tracked, 0, &ctx, .{ .slot = 1, .name = "foo" });

    const analyte2 = &tracked[1].typed_payload.?.immediate;
    try std.testing.expectEqual(.variable, std.meta.activeTag(analyte2.memory_safety.?.stack_ptr.name));
    try std.testing.expectEqualStrings("foo", analyte2.memory_safety.?.stack_ptr.name.variable);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;

    // Set up source with stack_ptr
    tracked[0].typed_payload = .{ .immediate = .{ .memory_safety = .{ .stack_ptr = .{
        .function = "source_func",
        .file = "source.zig",
        .line = 42,
        .column = 7,
        .name = .{ .variable = "src_var" },
    } } } };

    try MemorySafety.bitcast(&tracked, 1, &ctx, .{ .src = 0 });

    const analyte = &tracked[1].typed_payload.?.immediate;
    const ms = analyte.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.line);
    try std.testing.expectEqualStrings("src_var", ms.stack_ptr.name.variable);
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    var retval = Slot{};

    // Slot with stack_ptr from test_func (current function)
    tracked[0].typed_payload = .{ .immediate = .{ .memory_safety = .{ .stack_ptr = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 5,
        .name = .{ .variable = "local" },
    } } } };

    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(&tracked, 1, &ctx, .{ .src = 0, .retval_ptr = &retval }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var ctx = MockContext.init(allocator);
    defer ctx.deinit();

    var tracked = [_]Slot{.{}} ** 3;
    var retval = Slot{};

    // Slot with empty function name (arg)
    tracked[0].typed_payload = .{ .immediate = .{ .memory_safety = .{ .stack_ptr = .{
        .function = "",
        .file = "test.zig",
        .line = 5,
        .name = .{ .parameter = "param" },
    } } } };

    // Should NOT error - returning arg is fine
    try MemorySafety.ret_safe(&tracked, 1, &ctx, .{ .src = 0, .retval_ptr = &retval });
}
