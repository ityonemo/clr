const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

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

pub const Free = struct {
    meta: Meta,
    name_at_free: ?[]const u8 = null,
};

pub const Allocation = struct {
    allocated: Meta,
    freed: ?Free = null, // null = still allocated, has value = freed
    allocator_type: []const u8, // Type of allocator that created this allocation
    name_at_alloc: ?[]const u8 = null, // Access path when allocated (e.g., "container.ptr")
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
        // ptr is null for stores to interned/global locations - no memory safety tracking needed
        const ptr = params.ptr orelse return;
        const src = switch (params.src) {
            .eidx => |idx| idx,
            // comptime/global values don't have memory safety tracking - skip
            .interned, .other => return,
        };

        // When storing a pointer with allocation tracking, transfer ownership to the destination.
        // Copy the allocation metadata to the destination, then mark source as .passed.
        // This prevents false positives when the allocation is freed via the destination.
        const src_ref = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_ref);
        if (src_refinement.* == .pointer) {
            if (src_refinement.pointer.analyte.memory_safety) |*ms| {
                if (ms.* == .allocation) {
                    // Get the destination pointer's pointee (where the allocation will live)
                    const dst_ref = results[ptr].refinement orelse return;
                    const dst_refinement = refinements.at(dst_ref);
                    if (dst_refinement.* != .pointer) return;
                    const pointee_idx = dst_refinement.pointer.to;
                    const pointee = refinements.at(pointee_idx);
                    if (pointee.* == .pointer) {
                        // Copy allocation metadata to destination, capturing name from destination instruction
                        var new_alloc = ms.allocation;
                        new_alloc.name_at_alloc = results[ptr].name;
                        pointee.pointer.analyte.memory_safety = .{ .allocation = new_alloc };
                    }
                    // Mark source as passed - ownership transferred to destination
                    ms.* = .passed;
                }
            }
        }

        // If storing from a parameter, propagate the parameter name and location to the destination's stack_ptr
        const param_name = results[src].name orelse return;
        // Get the target pointer refinement (created by arg for pointer parameters)
        const tgt_refinement_idx = results[ptr].refinement orelse return;
        if (refinements.at(tgt_refinement_idx).* != .pointer) return;
        const tgt_ptr = &refinements.at(tgt_refinement_idx).pointer;

        if (tgt_ptr.analyte.memory_safety) |*ms| {
            if (ms.* == .stack_ptr) {
                ms.stack_ptr.name = .{ .parameter = param_name };
                ms.stack_ptr.meta = .{
                    .function = ctx.meta.function,
                    .file = ctx.meta.file,
                    .line = ctx.base_line + 1,
                    .column = null,
                };
            }
        }
    }

    /// Retroactively set variable name on stack_ptr for escape detection messages.
    /// The name is already set on the instruction by DbgVarPtr.apply().
    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        const inst = params.ptr orelse return;
        const ptr_idx = results[inst].refinement orelse return;
        if (refinements.at(ptr_idx).* == .pointer) {
            const outer_analyte = &refinements.at(ptr_idx).pointer.analyte;
            if (outer_analyte.memory_safety) |*ms| {
                if (ms.* == .stack_ptr) {
                    if (ms.stack_ptr.name == .other) {
                        ms.stack_ptr.name = .{ .variable = params.name };
                    }
                }
            }
        }
    }

    /// For interned args, no memory safety tracking needed (constants have no allocation state).
    /// For eidx args, memory safety state was already copied from caller.
    /// TODO: See LIMITATIONS.md - interned pointer args may need special handling.
    pub fn arg(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Arg) !void {
        _ = results;
        _ = index;
        _ = ctx;
        _ = refinements;
        _ = params;
        // Nothing to do - interned values have no memory safety state,
        // and eidx values already have their state copied from caller.
    }

    pub fn ret_safe(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.RetSafe) !void {
        _ = index;

        const src = switch (params.src) {
            .eidx => |idx| idx,
            // comptime/global values don't have memory safety tracking - skip
            .interned, .other => return,
        };
        // refinement is null for uninitialized instructions - skip (would be caught by undefined analysis)
        const src_idx = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_idx);

        // Only pointers can escape - scalars are copied by value, safe to return
        if (src_refinement.* != .pointer) return;

        // memory_safety is null for untracked pointers (e.g., external pointers) - skip
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
                        return reportMemoryLeak(ctx, allocation);
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
        // Result is errorunion -> ptr -> pointee
        const eu_idx = results[index].refinement.?;
        const ptr_idx = refinements.at(eu_idx).errorunion.to;
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
        // alloc_create always creates pointer refinement with memory_safety
        const ptr_analyte = &refinements.at(ptr_idx).pointer.analyte;
        const ms = &(ptr_analyte.memory_safety orelse @panic("alloc_destroy: no memory_safety"));

        switch (ms.*) {
            .stack_ptr => |sp| return reportFreeStackMemory(ctx, sp),
            .allocation => |a| {
                if (a.freed) |previous_free| {
                    return reportDoubleFree(ctx, a, previous_free);
                }
                if (!std.mem.eql(u8, a.allocator_type, params.allocator_type)) {
                    return reportMismatchedAllocator(ctx, a, params.allocator_type);
                }
                // Capture name from the pointer instruction being freed
                ms.allocation.freed = .{
                    .meta = ctx.meta,
                    .name_at_free = results[ptr].name,
                };
            },
            .passed => @panic("alloc_destroy on passed pointer - should not happen"),
        }
    }

    /// Handle load - detect use-after-free
    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        _ = index;
        // ptr is null for interned/global loads - no memory safety tracking needed
        const ptr = params.ptr orelse return;
        // refinement may be null for uninitialized instructions - skip
        const ptr_idx = results[ptr].refinement orelse return;
        const ptr_refinement = refinements.at(ptr_idx);

        // Loading through a non-pointer is a bug - we should only load through pointers
        if (ptr_refinement.* != .pointer) {
            std.debug.panic("memory_safety.load: expected pointer, got {s}", .{@tagName(ptr_refinement.*)});
        }

        // memory_safety may be null for stack allocations or untracked pointers
        const ms = ptr_refinement.pointer.analyte.memory_safety orelse return;
        // Only check use-after-free for heap allocations
        if (ms != .allocation) return;

        if (ms.allocation.freed) |free_site| {
            return reportUseAfterFree(ctx, ms.allocation, free_site);
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

    /// Format name prefix for allocation error messages.
    /// With name: "'container.ptr' "
    /// Without name: ""
    fn formatNamePrefix(name: ?[]const u8, buf: []u8) []const u8 {
        if (name) |n| {
            return std.fmt.bufPrint(buf, "'{s}' ", .{n}) catch "";
        }
        return "";
    }

    fn reportDoubleFree(ctx: *Context, allocation: Allocation, previous_free: Free) anyerror {
        try ctx.meta.print(ctx.writer, "double free in ", .{});
        // Use name_at_free for "previously freed" line
        var buf1: [256]u8 = undefined;
        const free_prefix = formatNamePrefix(previous_free.name_at_free, &buf1);
        if (free_prefix.len > 0) {
            try previous_free.meta.print(ctx.writer, "{s}previously freed in ", .{free_prefix});
        } else {
            try previous_free.meta.print(ctx.writer, "previously freed in ", .{});
        }
        // Use name_at_alloc for "originally allocated" line
        var buf2: [256]u8 = undefined;
        const alloc_prefix = formatNamePrefix(allocation.name_at_alloc, &buf2);
        if (alloc_prefix.len > 0) {
            try allocation.allocated.print(ctx.writer, "{s}originally allocated in ", .{alloc_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "originally allocated in ", .{});
        }
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: *Context, allocation: Allocation, free_site: Free) anyerror {
        try ctx.meta.print(ctx.writer, "use after free in ", .{});
        // Use name_at_free for "freed" line
        var buf1: [256]u8 = undefined;
        const free_prefix = formatNamePrefix(free_site.name_at_free, &buf1);
        if (free_prefix.len > 0) {
            try free_site.meta.print(ctx.writer, "{s}freed in ", .{free_prefix});
        } else {
            try free_site.meta.print(ctx.writer, "freed in ", .{});
        }
        // Use name_at_alloc for "allocated" line
        var buf2: [256]u8 = undefined;
        const alloc_prefix = formatNamePrefix(allocation.name_at_alloc, &buf2);
        if (alloc_prefix.len > 0) {
            try allocation.allocated.print(ctx.writer, "{s}allocated in ", .{alloc_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "allocated in ", .{});
        }
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: *Context, allocation: Allocation) anyerror {
        try ctx.meta.print(ctx.writer, "memory leak in ", .{});
        var buf: [256]u8 = undefined;
        const name_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
        if (name_prefix.len > 0) {
            try allocation.allocated.print(ctx.writer, "{s}allocated in ", .{name_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "allocated in ", .{});
        }
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

    // =========================================================================
    // Branch merging
    // =========================================================================

    /// Merge memory_safety state from two branches after a conditional.
    /// If one branch has memory_safety and the original doesn't, copy it.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) !void {
        _ = ctx;
        _ = merge_tag;
        _ = false_branch; // TODO: handle case where false branch has memory_safety
        mergeRefinement(orig, true_branch);
    }

    fn mergeRefinement(
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
    ) void {
        const orig_ref = orig[0].at(orig[1]);
        const true_ref = true_branch[0].at(true_branch[1]);

        switch (orig_ref.*) {
            .pointer => |*op| {
                if (true_ref.* == .pointer) {
                    const tp = true_ref.pointer;
                    // Copy memory_safety if original doesn't have it
                    if (op.analyte.memory_safety == null and tp.analyte.memory_safety != null) {
                        op.analyte.memory_safety = tp.analyte.memory_safety;
                    }
                    // Recursively merge pointee
                    mergeRefinement(.{ orig[0], op.to }, .{ true_branch[0], tp.to });
                }
            },
            .optional => |o| {
                if (true_ref.* == .optional) {
                    mergeRefinement(.{ orig[0], o.to }, .{ true_branch[0], true_ref.optional.to });
                }
            },
            .errorunion => |e| {
                if (true_ref.* == .errorunion) {
                    mergeRefinement(.{ orig[0], e.to }, .{ true_branch[0], true_ref.errorunion.to });
                }
            },
            .region => |r| {
                if (true_ref.* == .region) {
                    mergeRefinement(.{ orig[0], r.to }, .{ true_branch[0], true_ref.region.to });
                }
            },
            .@"struct" => |s| {
                if (true_ref.* == .@"struct") {
                    for (s.fields, true_ref.@"struct".fields) |of, tf| {
                        mergeRefinement(.{ orig[0], of }, .{ true_branch[0], tf });
                    }
                }
            },
            else => {},
        }
    }
};

// =========================================================================
// Validation
// =========================================================================

const debug = @import("builtin").mode == .Debug;

/// Validate that refinements conform to memory_safety tracking rules:
/// - Pointers and scalars CAN have memory_safety (scalars for ptr-to-int casts)
/// - Containers (optional, errorunion, struct) must NOT have memory_safety at the container level
pub fn testValid(refinements: *Refinements) void {
    if (!debug) return;
    for (refinements.list.items) |ref| {
        switch (ref) {
            .pointer, .scalar => {
                // Pointers CAN have memory_safety (stack_ptr or allocation)
                // Scalars CAN have memory_safety (pointer cast to integer)
                // No requirement that they MUST have it (untracked values are OK)
            },
            .optional => |o| {
                if (o.analyte.memory_safety != null) {
                    @panic("OptionalContainerHasMemorySafetyState");
                }
            },
            .errorunion => |e| {
                if (e.analyte.memory_safety != null) {
                    @panic("ErrorUnionContainerHasMemorySafetyState");
                }
            },
            .@"struct" => |s| {
                if (s.analyte.memory_safety != null) {
                    @panic("StructContainerHasMemorySafetyState");
                }
            },
            else => {},
        }
    }
}

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

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_eidx = 0,
        .caller_refinements = null,
    };
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
    const state = testState(&ctx, &results, &refinements);

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer) then MemorySafety.alloc
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

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
    const state = testState(&ctx, &results, &refinements);

    // First alloc to set up stack_ptr with .other name
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    const ms1 = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack_ptr.name));

    // dbg_var_ptr should set the variable name
    try Inst.apply(state, 2, .{ .dbg_var_ptr = .{ .ptr = 1, .name = "foo" } });

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
    const state = testState(&ctx, &results, &refinements);

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
    try Inst.apply(state, 1, .{ .bitcast = .{ .src = .{ .eidx = 0 } } });

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
        MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .src = .{ .eidx = 0 } }),
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
    try MemorySafety.ret_safe(&results, 1, &ctx, &refinements, .{ .src = .{ .eidx = 0 } });
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
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 1, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });

    // alloc_create creates errorunion -> ptr -> pointee
    const eu_idx = results[1].refinement.?;
    const ptr_idx = refinements.at(eu_idx).errorunion.to;
    const ms = refinements.at(ptr_idx).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.allocation, std.meta.activeTag(ms));
    try std.testing.expectEqualStrings("PageAllocator", ms.allocation.allocator_type);
    try std.testing.expectEqualStrings("test.zig", ms.allocation.allocated.file);
    try std.testing.expectEqual(@as(u32, 10), ms.allocation.allocated.line);
    try std.testing.expectEqual(@as(?Free, null), ms.allocation.freed);
}

test "alloc_destroy marks allocation as freed" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation (errorunion -> ptr -> pointee)
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    // Unwrap error union to get the pointer (simulating real AIR flow)
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

    // Update context for free location
    ctx.meta.line = 20;

    // Destroy allocation (ptr points to unwrapped pointer at inst 1)
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "PageAllocator" } });

    const ptr_idx = results[1].refinement.?;
    const ms = refinements.at(ptr_idx).pointer.analyte.memory_safety.?;
    try std.testing.expect(ms.allocation.freed != null);
    try std.testing.expectEqual(@as(u32, 20), ms.allocation.freed.?.meta.line);
}

test "alloc_destroy detects double free" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // First free
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "PageAllocator" } });

    // Second free should error
    try std.testing.expectError(
        error.DoubleFree,
        Inst.apply(state, 3, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "PageAllocator" } }),
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

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create with PageAllocator and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

    // Destroy with different allocator
    try std.testing.expectError(
        error.MismatchedAllocator,
        Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "ArenaAllocator" } }),
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
    const state = testState(&ctx, &results, &refinements);

    // Create stack allocation (alloc, not alloc_create)
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // Trying to free stack memory should error
    try std.testing.expectError(
        error.FreeStackMemory,
        Inst.apply(state, 1, .{ .alloc_destroy = .{ .ptr = 0, .allocator_type = "PageAllocator" } }),
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

    var results = [_]Inst{.{}} ** 6;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, store (to make it defined), and free allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });
    try Inst.apply(state, 3, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "PageAllocator" } });

    // Load after free should error
    try std.testing.expectError(
        error.UseAfterFree,
        Inst.apply(state, 4, .{ .load = .{ .ptr = 1, .ty = .{ .scalar = {} } } }),
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

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, and store to allocation (not freed)
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

    // Load from live allocation should succeed
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .scalar = {} } } });
}

test "onFinish detects memory leak" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation and unwrap but don't free
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

    // onFinish should detect the leak (via the unwrapped pointer at inst 1)
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

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, and free allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .allocator_type = "PageAllocator" } });

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
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    var results = [_]Inst{.{}} ** 5;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_eidx = return_eidx,
        .caller_refinements = &caller_refinements,
    };

    // Create allocation and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // Store to make the pointee defined
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

    // Return the pointer (marks as passed)
    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .eidx = 1 } } });

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
    const state = testState(&ctx, &results, &refinements);

    // Create stack allocation (not heap)
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // onFinish should not error - stack memory is fine
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "load from struct field shares pointer entity - freeing loaded pointer marks original as freed" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 11;
    const state = testState(&ctx, &results, &refinements);

    // === SETUP: struct with pointer field pointing to allocation ===
    // inst 0: alloc_create - create heap allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    // inst 1: unwrap_errunion_payload - get the pointer
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // inst 2: store_safe - make pointee defined
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

    // inst 3: alloc - create struct on stack with a pointer field
    const struct_ty: tag.Type = .{ .@"struct" = &.{.{ .ty = .{ .pointer = &.{ .scalar = {} } } }} };
    try Inst.apply(state, 3, .{ .alloc = .{ .ty = struct_ty } });
    // inst 4: store_safe - initialize struct with undefined
    try Inst.apply(state, 4, .{ .store_safe = .{ .ptr = 3, .src = .{ .interned = .{ .undefined = &struct_ty } } } });

    // inst 5: struct_field_ptr - get pointer to field 0
    try Inst.apply(state, 5, .{ .struct_field_ptr = .{
        .base = 3,
        .field_index = 0,
        .ty = .{ .pointer = &.{ .pointer = &.{ .scalar = {} } } },
    } });
    // inst 6: store_safe - store the allocation pointer into struct field
    try Inst.apply(state, 6, .{ .store_safe = .{ .ptr = 5, .src = .{ .eidx = 1 } } });

    // === LOAD POINTER FROM STRUCT FIELD ===
    // inst 7: load - load struct from stack alloc
    try Inst.apply(state, 7, .{ .load = .{ .ptr = 3, .ty = struct_ty } });
    // inst 8: struct_field_val - get pointer field value
    try Inst.apply(state, 8, .{ .struct_field_val = .{ .operand = 7, .field_index = 0, .ty = .{ .pointer = &.{ .scalar = {} } } } });

    // === FREE THE LOADED POINTER ===
    // inst 9: alloc_destroy - free via the loaded pointer
    try Inst.apply(state, 9, .{ .alloc_destroy = .{ .ptr = 8, .allocator_type = "PageAllocator" } });

    // === VERIFICATION ===
    // The original struct field pointer (stored via inst 6) should also be marked as freed.
    // This requires that load shares the pointer entity instead of copying it.
    //
    // Load the struct again and get the field - check its memory_safety state
    try Inst.apply(state, 10, .{ .load = .{ .ptr = 3, .ty = struct_ty } });

    // Get the struct field pointer from the original struct (via struct_field_ptr at inst 5)
    const field_ptr_ref = results[5].refinement.?;
    const field_ptr = refinements.at(field_ptr_ref);

    // The field pointer points to the pointer entity that should now be freed
    const ptr_entity_idx = field_ptr.pointer.to;
    const ptr_entity = refinements.at(ptr_entity_idx);

    // The pointer should be marked as freed (its memory_safety should have freed != null)
    try std.testing.expect(ptr_entity.* == .pointer);
    const ms = ptr_entity.pointer.analyte.memory_safety orelse {
        return error.ExpectedMemorySafetyState;
    };
    try std.testing.expect(ms == .allocation);
    // This is the key assertion: the allocation should be marked as freed
    try std.testing.expect(ms.allocation.freed != null);
}

test "interprocedural: callee freeing struct pointer field propagates back to caller" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    // === CALLER SETUP ===
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();

    var caller_results = [_]Inst{.{}} ** 6;
    const caller_state = testState(&ctx, &caller_results, &caller_refinements);

    // Caller: inst 0 = alloc_create, inst 1 = unwrap to get pointer
    try Inst.apply(caller_state, 0, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });
    try Inst.apply(caller_state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // inst 1 now has the allocation pointer

    // Caller: inst 2 = alloc struct with pointer field
    const struct_ty: tag.Type = .{ .@"struct" = &.{.{ .ty = .{ .pointer = &.{ .scalar = {} } } }} };
    try Inst.apply(caller_state, 2, .{ .alloc = .{ .ty = struct_ty } });
    // inst 3 = store undefined struct
    try Inst.apply(caller_state, 3, .{ .store_safe = .{ .ptr = 2, .src = .{ .interned = .{ .undefined = &struct_ty } } } });
    // inst 4 = struct_field_ptr to field 0
    try Inst.apply(caller_state, 4, .{ .struct_field_ptr = .{ .base = 2, .field_index = 0, .ty = .{ .pointer = &.{ .pointer = &.{ .scalar = {} } } } } });
    // inst 5 = store allocation pointer into struct field
    try Inst.apply(caller_state, 5, .{ .store_safe = .{ .ptr = 4, .src = .{ .eidx = 1 } } });

    // Get the allocation entity for later verification
    const alloc_ptr_ref = caller_results[1].refinement.?;
    const alloc_scalar_idx = caller_refinements.at(alloc_ptr_ref).pointer.to;

    // Verify allocation is not freed yet
    const alloc_scalar_before = caller_refinements.at(alloc_scalar_idx);
    try std.testing.expect(alloc_scalar_before.* == .scalar);
    // The allocation tracking is on the pointer, not the scalar
    const alloc_ptr_ms = caller_refinements.at(alloc_ptr_ref).pointer.analyte.memory_safety.?;
    try std.testing.expect(alloc_ptr_ms == .passed); // Marked as passed when stored to field

    // But the struct field pointer should point to the same scalar
    const struct_ptr_ref = caller_results[2].refinement.?;
    const struct_idx = caller_refinements.at(struct_ptr_ref).pointer.to;
    const field_ptr_idx = caller_refinements.at(struct_idx).@"struct".fields[0];
    const field_ptr = caller_refinements.at(field_ptr_idx);
    try std.testing.expect(field_ptr.* == .pointer);
    // The field pointer should have the allocation state
    const field_ptr_ms = field_ptr.pointer.analyte.memory_safety.?;
    try std.testing.expect(field_ptr_ms == .allocation);
    try std.testing.expect(field_ptr_ms.allocation.freed == null);

    // === CALLEE SETUP ===
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();

    var callee_results = [_]Inst{.{}} ** 5;

    // Copy caller's struct pointer using copy_to (simulating arg)
    const caller_ptr_idx = caller_results[2].refinement.?;
    const local_ptr_idx = try caller_refinements.at(caller_ptr_idx).*.copy_to(&caller_refinements, &callee_refinements);
    callee_results[0].refinement = local_ptr_idx;
    callee_results[0].caller_eidx = caller_ptr_idx;
    callee_results[0].name = "container";

    const callee_state = State{
        .ctx = &ctx,
        .results = &callee_results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // Callee: inst 1 = struct_field_ptr to get pointer to field 0
    try Inst.apply(callee_state, 1, .{ .struct_field_ptr = .{ .base = 0, .field_index = 0, .ty = .{ .pointer = &.{ .pointer = &.{ .scalar = {} } } } } });
    // Callee: inst 2 = load to get the pointer value from field
    try Inst.apply(callee_state, 2, .{ .load = .{ .ptr = 1, .ty = .{ .pointer = &.{ .scalar = {} } } } });
    // Callee: inst 3 = alloc_destroy to free the pointer
    try Inst.apply(callee_state, 3, .{ .alloc_destroy = .{ .ptr = 2, .allocator_type = "PageAllocator" } });

    // Callee's local pointer should be freed
    const local_loaded_ptr_ref = callee_results[2].refinement.?;
    const local_loaded_ptr = callee_refinements.at(local_loaded_ptr_ref);
    try std.testing.expect(local_loaded_ptr.* == .pointer);
    const local_ms = local_loaded_ptr.pointer.analyte.memory_safety.?;
    try std.testing.expect(local_ms == .allocation);
    try std.testing.expect(local_ms.allocation.freed != null);

    // === BACKPROPAGATE ===
    Inst.backPropagate(callee_state);

    // === VERIFY CALLER'S STATE IS UPDATED ===
    // The caller's struct field pointer should now be marked as freed
    const field_ptr_after = caller_refinements.at(field_ptr_idx);
    try std.testing.expect(field_ptr_after.* == .pointer);
    const field_ptr_ms_after = field_ptr_after.pointer.analyte.memory_safety.?;
    try std.testing.expect(field_ptr_ms_after == .allocation);
    // This is the key assertion: the caller's field pointer should be marked as freed
    try std.testing.expect(field_ptr_ms_after.allocation.freed != null);
}
