const std = @import("std");
const tag = @import("tag.zig");
const Context = @import("Context.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const Analyte = Refinements.Analyte;
pub const EIdx = Refinements.EIdx;

const Inst = @This();
const ArgumentInfo = struct {
    caller_ref: EIdx,
    name: []const u8,
};

/// Index into the Refinements entity table, or null if not yet initialized.
refinement: ?EIdx = null,

/// For args: caller's entity index for backward propagation, and parameter name for error messages.
/// Used by backPropagate() along with the caller_refinements parameter.
argument: ?ArgumentInfo = null,

/// Get this instruction's Refinement. Crashes if not initialized.
pub fn get(self: *Inst, refinements: *Refinements) *Refinement {
    return refinements.at(self.refinement.?);
}

pub fn apply(index: usize, any_tag: tag.AnyTag, results: []Inst, ctx: *Context, refinements: *Refinements) !void {
    switch (any_tag) {
        inline else => |t| try t.apply(results, index, ctx, refinements),
    }
}

pub fn call(index: usize, called: anytype, args: anytype, results: []Inst, ctx: *Context, refinements: *Refinements) !void {
    // Skip if called is null (indirect call through function pointer - TODO: handle these)
    if (@TypeOf(called) == @TypeOf(null)) return;
    // Pass caller's refinements so callee can write return value into it
    const return_eidx = try @call(.auto, called, .{ ctx, refinements } ++ args);
    // Deposit returned entity index into caller's instruction
    results[index].refinement = return_eidx;
}

/// Execute both branches of a conditional, then merge results.
/// Both branches are executed to get conservative analysis results.
pub fn cond_br(
    comptime index: usize,
    comptime true_fn: fn ([]Inst, *Context, *Refinements, ?*Refinements, EIdx) anyerror!void,
    comptime false_fn: fn ([]Inst, *Context, *Refinements, ?*Refinements, EIdx) anyerror!void,
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    caller_refinements: ?*Refinements,
    return_eidx: EIdx,
) !void {
    // Clone results and refinements, and context for each branch
    const true_results = clone_results_list(results, ctx.allocator);
    defer clear_results_list(true_results, ctx.allocator);
    var true_refinements = try refinements.clone(ctx.allocator);
    defer true_refinements.deinit();
    const true_ctx = ctx.copy();
    defer true_ctx.delete();

    const false_results = clone_results_list(results, ctx.allocator);
    defer clear_results_list(false_results, ctx.allocator);
    var false_refinements = try refinements.clone(ctx.allocator);
    defer false_refinements.deinit();
    const false_ctx = ctx.copy();
    defer false_ctx.delete();

    // Clone caller_refinements for each branch so ret_safe writes don't conflict
    var true_caller_refinements: ?Refinements = if (caller_refinements) |cp|
        try cp.clone(ctx.allocator)
    else
        null;
    defer if (true_caller_refinements) |*r| r.deinit();

    var false_caller_refinements: ?Refinements = if (caller_refinements) |cp|
        try cp.clone(ctx.allocator)
    else
        null;
    defer if (false_caller_refinements) |*r| r.deinit();

    // Execute both branches (they modify their cloned state)
    try true_fn(true_results, true_ctx, &true_refinements, if (true_caller_refinements) |*r| r else null, return_eidx);
    try false_fn(false_results, false_ctx, &false_refinements, if (false_caller_refinements) |*r| r else null, return_eidx);

    // Mark the block instruction as void
    results[index].refinement = try refinements.appendEntity(.{ .void = {} });

    // Merge: walk results and call analysis merge for each slot that has refinements
    try tag.splatMerge(.cond_br, results, ctx, refinements, true_results, &true_refinements, false_results, &false_refinements);

    // Merge caller_refinements return slot if both branches wrote to it
    if (caller_refinements) |cp| {
        try mergeCallerReturn(
            ctx,
            cp,
            return_eidx,
            if (true_caller_refinements) |*r| r else null,
            if (false_caller_refinements) |*r| r else null,
        );
    }
}

/// Merge the return slot from both branch's caller_refinements back into the original.
/// Called after both branches of a conditional have executed.
fn mergeCallerReturn(
    ctx: *Context,
    caller_refinements: *Refinements,
    return_eidx: EIdx,
    true_caller: ?*Refinements,
    false_caller: ?*Refinements,
) !void {
    const true_cp = true_caller orelse return;
    const false_cp = false_caller orelse return;

    const orig_return = caller_refinements.at(return_eidx);
    const true_return = true_cp.at(return_eidx);
    const false_return = false_cp.at(return_eidx);

    // If original is still retval_future, check if either branch set it
    if (orig_return.* == .retval_future) {
        const true_set = true_return.* != .retval_future;
        const false_set = false_return.* != .retval_future;

        if (true_set and false_set) {
            // Both branches returned - need to merge
            // For now, copy true branch's value, then let analyses merge
            const true_idx = try true_return.*.copy_to(true_cp, caller_refinements);
            orig_return.* = caller_refinements.at(true_idx).*;

            // Call each analysis's merge function for the return value
            inline for (tag.analyses) |Analysis| {
                if (@hasDecl(Analysis, "mergeReturn")) {
                    try Analysis.mergeReturn(
                        ctx,
                        caller_refinements,
                        return_eidx,
                        true_cp,
                        false_cp,
                    );
                }
            }
        } else if (true_set) {
            // Only true branch returned
            const true_idx = try true_return.*.copy_to(true_cp, caller_refinements);
            orig_return.* = caller_refinements.at(true_idx).*;
        } else if (false_set) {
            // Only false branch returned
            const false_idx = try false_return.*.copy_to(false_cp, caller_refinements);
            orig_return.* = caller_refinements.at(false_idx).*;
        }
        // else: neither branch returned, leave as retval_future
    }
}

// =============================================================================
// Results list management
// =============================================================================

pub fn make_results_list(allocator: std.mem.Allocator, count: usize) []Inst {
    const list = allocator.alloc(Inst, count) catch @panic("out of memory");
    for (list) |*inst| {
        inst.* = .{};
    }
    return list;
}

pub fn clear_results_list(list: []Inst, allocator: std.mem.Allocator) void {
    allocator.free(list);
}

pub fn clone_results_list(list: []Inst, allocator: std.mem.Allocator) []Inst {
    const new_list = allocator.alloc(Inst, list.len) catch @panic("out of memory");
    @memcpy(new_list, list);
    return new_list;
}

// =============================================================================
// Refinement helpers
// =============================================================================

/// Initialize an instruction with a new scalar refinement. Crashes if already initialized.
pub fn initInst(refinements: *Refinements, results: []Inst, index: usize) !EIdx {
    if (results[index].refinement) |_| @panic("instruction already initialized");
    const idx: EIdx = @intCast(refinements.list.items.len);
    try refinements.list.append(.{ .scalar = .{} });
    results[index].refinement = idx;
    return idx;
}

/// Overwrite an instruction with a new refinement. Creates new entity regardless of prior state.
/// Call this in tag handlers before splat() so analyses can set their respective fields.
/// For conditional keep-of-previous-value semantics, use merge (not yet implemented).
pub fn clobberInst(refinements: *Refinements, results: []Inst, index: usize, value: Refinement) !EIdx {
    const idx: EIdx = @intCast(refinements.list.items.len);
    try refinements.list.append(value);
    results[index].refinement = idx;
    return idx;
}

// =============================================================================
// Function lifecycle
// =============================================================================

pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
    try tag.splatFinish(results, ctx, refinements);
}

/// Propagate analysis state back to callers via caller_ref.
/// Called after onFinish to copy state back to the caller's entity.
/// Propagates both:
/// - Pointer's analyte (memory_safety for allocation tracking)
/// - Pointee's analyte (undefined tracking for scalar values)
pub fn backPropagate(results: []Inst, refinements: *Refinements, caller_refinements: ?*Refinements) void {
    const cp = caller_refinements orelse return; // entrypoint, nothing to propagate
    for (results) |inst| {
        const arg_info = inst.argument orelse continue;
        const caller_entity_idx = arg_info.caller_ref;
        const local_idx = inst.refinement orelse continue;

        const local_refinement = refinements.at(local_idx);
        const caller_entity = cp.at(caller_entity_idx);

        switch (local_refinement.*) {
            .scalar => |src| {
                switch (caller_entity.*) {
                    .scalar => |*dst| dst.* = src,
                    else => {},
                }
            },
            .pointer => |local_ptr| {
                switch (caller_entity.*) {
                    .pointer => |*caller_ptr| {
                        // Propagate pointer's analyte (memory_safety)
                        caller_ptr.analyte = local_ptr.analyte;
                        // Propagate pointee's analyte (undefined tracking)
                        switch (refinements.at(local_ptr.to).*) {
                            .scalar => |src| switch (cp.at(caller_ptr.to).*) {
                                .scalar => |*dst| dst.* = src,
                                else => {},
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
    }
}

/// Assert all refinements are valid (non-null for all results that have one).
pub fn assertAllValid(refinements: *Refinements, results: []const Inst) void {
    for (results, 0..) |inst, i| {
        if (inst.refinement) |idx| {
            if (idx >= refinements.list.items.len) {
                std.debug.panic("instruction {} has invalid refinement index {}", .{ i, idx });
            }
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

test "alloc sets state to undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    try Inst.apply(0, .{ .dbg_stmt = .{ .line = 0, .column = 0 } }, results, &ctx, &refinements);
    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);

    // dbg_stmt sets instruction to void (no analysis tracking)
    try std.testing.expect(results[0].refinement != null);
    try std.testing.expectEqual(.void, std.meta.activeTag(results[0].get(&refinements).*));
    // alloc creates pointer; the pointee is marked undefined
    try std.testing.expect(results[1].refinement != null);
    try std.testing.expectEqual(.pointer, std.meta.activeTag(results[1].get(&refinements).*));
    const pointee_idx = results[1].get(&refinements).pointer.to;
    const analyte = &refinements.at(pointee_idx).scalar;
    try std.testing.expect(analyte.undefined != null);
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
    // uninitialized instruction has no refinement yet
    try std.testing.expectEqual(null, results[2].refinement);
}

test "store_safe with undef keeps state undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);
    try Inst.apply(2, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = true } }, results, &ctx, &refinements);

    // alloc's pointee stays undefined after store_safe with undef
    const pointee_idx = results[1].get(&refinements).pointer.to;
    const analyte = &refinements.at(pointee_idx).scalar;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(analyte.undefined.?));
}

test "store_safe with value sets state to defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);
    try Inst.apply(2, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, results, &ctx, &refinements);

    // alloc's pointee becomes defined after store_safe with real value
    const pointee_idx = results[1].get(&refinements).pointer.to;
    const analyte = &refinements.at(pointee_idx).scalar;
    try std.testing.expectEqual(.defined, std.meta.activeTag(analyte.undefined.?));
}

test "load from undefined instruction reports use before assign" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    // Set up: alloc creates undefined instruction
    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);

    // Load from undefined instruction should return error
    try std.testing.expectError(error.UseBeforeAssign, Inst.apply(2, .{ .load = .{ .ptr = 1 } }, results, &ctx, &refinements));
}

test "load from defined instruction does not report error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 4);
    defer clear_results_list(results, allocator);

    // Set up: alloc then store a real value
    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);
    try Inst.apply(2, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, results, &ctx, &refinements);

    // Load from defined instruction should NOT return error
    try Inst.apply(3, .{ .load = .{ .ptr = 1 } }, results, &ctx, &refinements);
}

test "all instructions get valid refinements after operations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 5);
    defer clear_results_list(results, allocator);

    // Apply various operations that should all set refinements
    try Inst.apply(0, .{ .dbg_stmt = .{ .line = 0, .column = 0 } }, results, &ctx, &refinements);
    try Inst.apply(1, .{ .alloc = .{} }, results, &ctx, &refinements);
    try Inst.apply(2, .{ .store_safe = .{ .ptr = 1, .src = null, .is_undef = false } }, results, &ctx, &refinements);
    try Inst.apply(3, .{ .load = .{ .ptr = 1 } }, results, &ctx, &refinements);
    try Inst.apply(4, .{ .block = .{} }, results, &ctx, &refinements);

    // All instructions should have valid refinements
    assertAllValid(&refinements, results);
}

test "ret_safe copies scalar return value to caller_refinements" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func"); // ret_safe needs a function name on stacktrace

    // Callee's refinements and results
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_results = make_results_list(allocator, 3);
    defer clear_results_list(callee_results, allocator);

    // Caller's refinements - pre-allocate return entity
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    // Verify return entity is initially unset
    try std.testing.expectEqual(.retval_future, std.meta.activeTag(caller_refinements.at(return_eidx).*));

    // In callee: allocate and store a value
    try Inst.apply(0, .{ .alloc = .{} }, callee_results, &ctx, &callee_refinements);
    try Inst.apply(1, .{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, callee_results, &ctx, &callee_refinements);

    // Return the value from instruction 0
    try Inst.apply(2, .{ .ret_safe = .{
        .caller_refinements = &caller_refinements,
        .return_eidx = return_eidx,
        .src = 0,
    } }, callee_results, &ctx, &callee_refinements);

    // Verify return entity in caller's refinements is now a pointer
    try std.testing.expectEqual(.pointer, std.meta.activeTag(caller_refinements.at(return_eidx).*));
}

test "ret_safe with null src sets caller return to void" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func");

    // Callee's refinements and results
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_results = make_results_list(allocator, 1);
    defer clear_results_list(callee_results, allocator);

    // Caller's refinements - pre-allocate return entity
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    // Return void (src = null)
    try Inst.apply(0, .{ .ret_safe = .{
        .caller_refinements = &caller_refinements,
        .return_eidx = return_eidx,
        .src = null,
    } }, callee_results, &ctx, &callee_refinements);

    // Verify return entity in caller's refinements is now void
    try std.testing.expectEqual(.void, std.meta.activeTag(caller_refinements.at(return_eidx).*));
}

test "ret_safe with null caller_refinements (entrypoint) succeeds" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func");

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();
    const results = make_results_list(allocator, 2);
    defer clear_results_list(results, allocator);

    // Allocate a value
    try Inst.apply(0, .{ .alloc = .{} }, results, &ctx, &refinements);
    try Inst.apply(1, .{ .store_safe = .{ .ptr = 0, .src = null, .is_undef = false } }, results, &ctx, &refinements);

    // Return with null caller_refinements (entrypoint case) - should just succeed without error
    try Inst.apply(1, .{
        .ret_safe = .{
            .caller_refinements = null,
            .return_eidx = 0, // doesn't matter when caller_refinements is null
            .src = 0,
        },
    }, results, &ctx, &refinements);
}

// =============================================================================
// backPropagate Tests
// =============================================================================

test "backPropagate with null caller_refinements does nothing" {
    const allocator = std.testing.allocator;

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);

    // Should just return without error
    backPropagate(results, &refinements, null);
}

test "backPropagate propagates scalar analyte to caller" {
    const allocator = std.testing.allocator;
    const undefined_analysis = @import("analysis/undefined.zig");

    // Set up caller's refinements with a scalar
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const caller_scalar_idx = try caller_refinements.appendEntity(.{ .scalar = .{} });

    // Set up callee's refinements with a modified scalar
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_scalar_idx = try callee_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .defined = {} },
    } });

    // Set up results with argument info pointing to caller
    const results = make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_scalar_idx;
    results[0].argument = .{ .caller_ref = caller_scalar_idx, .name = "arg" };

    // Verify caller scalar is initially undefined tracking = null
    try std.testing.expectEqual(@as(?undefined_analysis.Undefined, null), caller_refinements.at(caller_scalar_idx).scalar.undefined);

    // Propagate
    backPropagate(results, &callee_refinements, &caller_refinements);

    // Verify caller scalar now has defined state
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_scalar_idx).scalar.undefined.?));
}

test "backPropagate propagates pointer analyte to caller" {
    const allocator = std.testing.allocator;
    const memory_safety = @import("analysis/memory_safety.zig");

    // Set up caller's refinements with a pointer (no memory_safety)
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const caller_pointee_idx = try caller_refinements.appendEntity(.{ .scalar = .{} });
    const caller_ptr_idx = try caller_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{},
        .to = caller_pointee_idx,
    } });

    // Set up callee's refinements with a pointer that has memory_safety set
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_pointee_idx = try callee_refinements.appendEntity(.{ .scalar = .{} });
    const callee_ptr_idx = try callee_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .allocation = .{
            .allocated = .{ .function = "test", .file = "test.zig", .line = 1 },
            .allocator_type = "TestAllocator",
        } } },
        .to = callee_pointee_idx,
    } });

    // Set up results with argument info
    const results = make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_ptr_idx;
    results[0].argument = .{ .caller_ref = caller_ptr_idx, .name = "ptr" };

    // Verify caller pointer has no memory_safety initially
    try std.testing.expectEqual(@as(?memory_safety.MemorySafety, null), caller_refinements.at(caller_ptr_idx).pointer.analyte.memory_safety);

    // Propagate
    backPropagate(results, &callee_refinements, &caller_refinements);

    // Verify caller pointer now has memory_safety
    const ms = caller_refinements.at(caller_ptr_idx).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.allocation, std.meta.activeTag(ms));
    try std.testing.expectEqualStrings("TestAllocator", ms.allocation.allocator_type);
}

test "backPropagate propagates pointee undefined state to caller" {
    const allocator = std.testing.allocator;

    // Set up caller's refinements with a pointer to undefined scalar
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const caller_pointee_idx = try caller_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .undefined = .{
            .meta = .{ .function = "", .file = "", .line = 0 },
        } },
    } });
    const caller_ptr_idx = try caller_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{},
        .to = caller_pointee_idx,
    } });

    // Set up callee's refinements with pointer to defined scalar
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_pointee_idx = try callee_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .defined = {} },
    } });
    const callee_ptr_idx = try callee_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{},
        .to = callee_pointee_idx,
    } });

    // Set up results with argument info
    const results = make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_ptr_idx;
    results[0].argument = .{ .caller_ref = caller_ptr_idx, .name = "ptr" };

    // Verify caller's pointee is undefined initially
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_pointee_idx).scalar.undefined.?));

    // Propagate
    backPropagate(results, &callee_refinements, &caller_refinements);

    // Verify caller's pointee is now defined
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_pointee_idx).scalar.undefined.?));
}
