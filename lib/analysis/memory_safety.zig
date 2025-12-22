const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
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
    allocator_type: []const u8, // Type of allocator that created this allocation
};

pub const MemorySafety = union(enum) {
    stack_ptr: StackPtr,
    allocation: Allocation,
    passed: void, 

    pub fn alloc(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, set memory_safety on the pointer's analyte
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.memory_safety = .{ .stack_ptr = .{ .meta = ctx.meta } };
    }

    pub fn store(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Store) !void {
        _ = index;
        // TODO: let this handle comptime case
        const ptr = params.ptr orelse return;
        const src = params.src orelse return;

        // If storing from a parameter, propagate the parameter name and location to the destination's stack_ptr
        if (results[src].argument) |arg_info| {
            // get the target pointer refinement (this should generally have been created by "arg")
            const tgt_refinement_idx = results[ptr].refinement orelse @panic("store: ptr parameter has no refinement");
            const tgt_refinement = refinements.at(tgt_refinement_idx);

            if (tgt_refinement.* != .pointer) @panic("store: non-pointer target value");

            if (tgt_refinement.pointer.analyte.memory_safety) |*ms| {
                if (ms.* == .stack_ptr) {
                    ms.stack_ptr.name = .{ .parameter = arg_info.name };
                    ms.stack_ptr.meta = .{
                        .function = ctx.meta.function,
                        .file = ctx.meta.file,
                        .line = ctx.base_line + 1,
                        .column = null,
                    };
                }
            }
            else @panic("store: no memory safety initialized");
        }
    }

    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        // Set the variable name on the stack_ptr metadata (now on pointer's analyte)
        const inst = params.ptr orelse return;
        std.debug.assert(inst < results.len);
        const ptr_idx = results[inst].refinement orelse return;
        // Get the pointer's analyte and update memory_safety
        const src_refinement = refinements.at(ptr_idx);
        if (src_refinement.* != .pointer) @panic("non-pointer src for dbg_var_ptr");

        const ms = &(src_refinement.pointer.analyte.memory_safety orelse @panic("dbg_var_ptr: no memory safety initialized"));
        if (ms.* != .stack_ptr) return;
        if (ms.stack_ptr.name == .other) {
            ms.stack_ptr.name = .{ .variable = params.name };
        }
    }

    pub fn ret_safe(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.RetSafe) !void {
        _ = index;

        const src = params.src orelse return;
        std.debug.assert(src < results.len);

        const src_idx = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_idx);

        // Only pointers can escape - scalars are fine
        if (src_refinement.* != .pointer) return;

        const ms = &(src_refinement.pointer.analyte.memory_safety orelse return);

        switch (ms.*) {
            .stack_ptr => |sp| {
                // Only flag as escape if stack_ptr is from this function
                const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                if (std.mem.eql(u8, sp.meta.function, func_name)) {
                    return reportStackEscape(ms.*, ctx);
                }
            },
            .allocation => {
                // Mark as passed - ownership transferred to caller
                ms.* = .passed;
            },
            // TODO: When branches have their own entity lists, .passed here should be a @panic
            // (can't return the same allocation twice from different branches)
            .passed => {},
        }
    }

    /// Called on function close to check for memory leaks.
    /// Backward propagation is handled centrally by Inst.backPropagate().
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
        // Check for memory leaks
        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            const refinement = refinements.at(idx);
            if (refinement.* != .pointer) continue;

            const ms = refinement.pointer.analyte.memory_safety orelse continue;
            switch (ms) {
                .allocation => |allocation| {
                    if (allocation.freed == null) {
                        return reportMemoryLeak(ctx, allocation.allocated);
                    }
                },
                .passed => {}, // Ownership transferred to caller - not a leak
                .stack_ptr => {},
            }
        }
    }

    // =========================================================================
    // Allocation tracking (use-after-free, double-free, memory leak detection)
    // =========================================================================

    /// Handle allocator.create() - marks pointer as heap allocation
    pub fn alloc_create(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocCreate) !void {
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.memory_safety = .{ .allocation = .{
            .allocated = ctx.meta,
            .allocator_type = params.allocator_type,
        } };
    }

    /// Handle allocator.destroy() - marks as freed, detects double-free and mismatched allocator
    pub fn alloc_destroy(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocDestroy) !void {
        _ = index;
        const ptr = params.ptr;

        const ptr_idx = results[ptr].refinement orelse @panic("alloc_destroy: inst has no refinement");
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) @panic("alloc_destroy: expected pointer type");

        const ms = &(ptr_refinement.pointer.analyte.memory_safety orelse @panic("alloc_destroy: no memory_safety"));

        switch (ms.*) {
            .stack_ptr => |sp| return reportFreeStackMemory(ctx, sp),
            .allocation => |a| {
                if (a.freed) |previous_free| {
                    return reportDoubleFree(ctx, a.allocated, previous_free);
                }
                if (!std.mem.eql(u8, a.allocator_type, params.allocator_type)) {
                    return reportMismatchedAllocator(ctx, a, params.allocator_type);
                }
                ms.allocation.freed = ctx.meta;
            },
            .passed => @panic("alloc_destroy on passed pointer - should not happen"),
        }
    }

    /// Handle load - detect use-after-free
    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        _ = index;
        const ptr = params.ptr orelse return;
        std.debug.assert(ptr < results.len);

        const ptr_idx = results[ptr].refinement orelse return;
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) return;

        const ms = ptr_refinement.pointer.analyte.memory_safety orelse return;
        if (ms != .allocation) return;

        if (ms.allocation.freed) |free_site| {
            return reportUseAfterFree(ctx, ms.allocation.allocated, free_site);
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

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32, base_line: u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
    ctx.base_line = base_line;
    return ctx;
}

test "alloc sets stack_ptr metadata on pointer analyte" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer) then MemorySafety.alloc
    try Inst.apply(1, .{ .alloc = .{} }, &results, &ctx, &refinements);

    const ms = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.meta.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // First alloc to set up stack_ptr with .other name
    try Inst.apply(1, .{ .alloc = .{} }, &results, &ctx, &refinements);
    const ms1 = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try Inst.apply(2, .{ .dbg_var_ptr = .{ .ptr = 1, .name = "foo" } }, &results, &ctx, &refinements);

    const ms2 = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.variable, std.meta.activeTag(ms2.stack_ptr.name));
    try std.testing.expectEqualStrings("foo", ms2.stack_ptr.name.variable);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Set up source pointer with stack_ptr on analyte
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{} });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "source_func",
                .file = "source.zig",
                .line = 42,
                .column = 7,
            },
            .name = .{ .variable = "src_var" },
        } } },
        .to = pointee_idx,
    } });

    // Bitcast shares the refinement
    try Inst.apply(1, .{ .bitcast = .{ .src = 0 } }, &results, &ctx, &refinements);

    const ms = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.meta.line);
    try std.testing.expectEqualStrings("src_var", ms.stack_ptr.name.variable);
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Pointer with stack_ptr from test_func (current function)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{} });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "test_func",
                .file = "test.zig",
                .line = 5,
            },
            .name = .{ .variable = "local" },
        } } },
        .to = pointee_idx,
    } });

    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .caller_refinements = null, .return_eidx = 0, .src = 0 }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Pointer with empty function name (from caller via arg)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{} });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "",
                .file = "test.zig",
                .line = 5,
            },
            .name = .{ .parameter = "param" },
        } } },
        .to = pointee_idx,
    } });

    // Should NOT error - returning pointer from caller is fine
    try MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .caller_refinements = null, .return_eidx = 0, .src = 0 });
}

test "alloc_create sets allocation metadata on pointer analyte" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    try Inst.apply(1, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    const ms = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.allocation, std.meta.activeTag(ms));
    try std.testing.expectEqualStrings("PageAllocator", ms.allocation.allocator_type);
    try std.testing.expectEqualStrings("test.zig", ms.allocation.allocated.file);
    try std.testing.expectEqual(@as(u32, 10), ms.allocation.allocated.line);
    try std.testing.expectEqual(@as(?Meta, null), ms.allocation.freed);
}

test "alloc_destroy marks allocation as freed" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create allocation
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // Update context for free location
    ctx.meta.line = 20;

    // Destroy allocation
    try Inst.apply(1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    const ms = refinements.at(results[0].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expect(ms.allocation.freed != null);
    try std.testing.expectEqual(@as(u32, 20), ms.allocation.freed.?.line);
}

test "alloc_destroy detects double free" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create and free allocation
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);
    try Inst.apply(1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // Second free should error
    try std.testing.expectError(
        error.DoubleFree,
        Inst.apply(2, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements),
    );
}

test "alloc_destroy detects mismatched allocator" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create with PageAllocator
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // Destroy with different allocator
    try std.testing.expectError(
        error.MismatchedAllocator,
        Inst.apply(1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "ArenaAllocator" } }, &results, &ctx, &refinements),
    );
}

test "alloc_destroy detects freeing stack memory" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create stack allocation (alloc, not alloc_create)
    try Inst.apply(0, .{ .alloc = .{} }, &results, &ctx, &refinements);

    // Trying to free stack memory should error
    try std.testing.expectError(
        error.FreeStackMemory,
        Inst.apply(1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements),
    );
}

test "load detects use after free" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;

    // Create, store (to make it defined), and free allocation
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);
    try Inst.apply(1, .{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, &results, &ctx, &refinements);
    try Inst.apply(2, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // Load after free should error
    try std.testing.expectError(
        error.UseAfterFree,
        Inst.apply(3, .{ .load = .{ .ptr = 0 } }, &results, &ctx, &refinements),
    );
}

test "load from live allocation does not error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create and store to allocation (not freed)
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);
    try Inst.apply(1, .{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, &results, &ctx, &refinements);

    // Load from live allocation should succeed
    try Inst.apply(2, .{ .load = .{ .ptr = 0 } }, &results, &ctx, &refinements);
}

test "onFinish detects memory leak" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;

    // Create allocation but don't free
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // onFinish should detect the leak
    try std.testing.expectError(
        error.MemoryLeak,
        MemorySafety.onFinish(&results, &ctx, &refinements),
    );
}

test "onFinish allows freed allocation" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;

    // Create and free allocation
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);
    try Inst.apply(1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // onFinish should not error
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "onFinish allows passed allocation" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.initEntity();

    var results = [_]Inst{.{}} ** 2;

    // Create allocation
    try Inst.apply(0, .{ .alloc_create = .{ .allocator_type = "PageAllocator" } }, &results, &ctx, &refinements);

    // Return it (marks as passed)
    try Inst.apply(1, .{ .ret_safe = .{ .caller_refinements = &caller_refinements, .return_eidx = return_eidx, .src = 0 } }, &results, &ctx, &refinements);

    // onFinish should not error - allocation was passed to caller
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "onFinish ignores stack allocations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;

    // Create stack allocation (not heap)
    try Inst.apply(0, .{ .alloc = .{} }, &results, &ctx, &refinements);

    // onFinish should not error - stack memory is fine
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}
