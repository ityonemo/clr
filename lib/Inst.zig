const std = @import("std");
const tag = @import("tag.zig");
const Context = @import("Context.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const Analyte = Refinements.Analyte;
pub const EIdx = Refinements.EIdx;
pub const State = @import("lib.zig").State;

const Inst = @This();

/// Index into the Refinements entity table, or null if not yet initialized.
refinement: ?EIdx = null,

/// For args: caller's entity index for backward propagation.
/// Null for interned args (compile-time constants have nothing to propagate back to).
/// Used by backPropagate() along with the caller_refinements parameter.
caller_eidx: ?EIdx = null,

/// Access path for this instruction's result (e.g., "container.ptr").
/// Set by dbg_var_ptr for variable names, and by struct_field_ptr for field paths.
/// this could be either a variable name or a variable name + various fields.
name: ?[]const u8 = null,

/// Get this instruction's Refinement. Crashes if not initialized.
pub fn get(self: *Inst, refinements: *Refinements) *Refinement {
    return refinements.at(self.refinement.?);
}

pub fn apply(state: State, index: usize, any_tag: tag.AnyTag) !void {
    switch (any_tag) {
        inline else => |t| try t.apply(state, index),
    }
}

pub fn call(state: State, index: usize, called: anytype, args: anytype) !void {
    // Skip if called is null (indirect call through function pointer - TODO: handle these)
    if (@TypeOf(called) == @TypeOf(null)) return;
    // Save caller's base_line - callee will set its own
    const saved_base_line = state.ctx.base_line;
    // Pass caller's refinements so callee can write return value into it
    const return_eidx = try @call(.auto, called, .{ state.ctx, state.refinements } ++ args);
    // Restore caller's base_line
    state.ctx.base_line = saved_base_line;
    // Deposit returned entity index into caller's instruction
    state.results[index].refinement = return_eidx;
}

/// Execute both branches of a conditional, then merge results.
/// Both branches are executed to get conservative analysis results.
pub fn cond_br(
    state: State,
    comptime index: usize,
    comptime true_fn: fn (State) anyerror!void,
    comptime false_fn: fn (State) anyerror!void,
) !void {
    const ctx = state.ctx;
    const results = state.results;
    const refinements = state.refinements;
    const caller_refinements = state.caller_refinements;
    const return_eidx = state.return_eidx;

    // Clone results and refinements, and context for each branch
    const true_results = try clone_results_list(results, ctx.allocator);
    defer clear_results_list(true_results, ctx.allocator);
    var true_refinements = try refinements.clone(ctx.allocator);
    defer true_refinements.deinit();
    const true_ctx = try ctx.copy();
    defer true_ctx.delete();

    const false_results = try clone_results_list(results, ctx.allocator);
    defer clear_results_list(false_results, ctx.allocator);
    var false_refinements = try refinements.clone(ctx.allocator);
    defer false_refinements.deinit();
    const false_ctx = try ctx.copy();
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

    // Build state for each branch
    const true_state = State{
        .ctx = true_ctx,
        .results = true_results,
        .refinements = &true_refinements,
        .caller_refinements = if (true_caller_refinements) |*r| r else null,
        .return_eidx = return_eidx,
    };
    const false_state = State{
        .ctx = false_ctx,
        .results = false_results,
        .refinements = &false_refinements,
        .caller_refinements = if (false_caller_refinements) |*r| r else null,
        .return_eidx = return_eidx,
    };

    // Execute both branches (they modify their cloned state)
    try true_fn(true_state);
    try false_fn(false_state);

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
    // Caller refinements are null when cond_br is at entrypoint (no caller to propagate to).
    // This is legitimate - entrypoint functions have no caller return slot to merge.
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
        }
        // If only one branch returned (early return), don't copy it.
        // The other branch continues to code after cond_br, which will set its own return.
        // If neither branch returned, leave as retval_future.
    }
}

// =============================================================================
// Results list management
// =============================================================================

pub fn make_results_list(allocator: std.mem.Allocator, count: usize) error{OutOfMemory}![]Inst {
    const list = try allocator.alloc(Inst, count);
    for (list) |*inst| {
        inst.* = .{};
    }
    return list;
}

pub fn clear_results_list(list: []Inst, allocator: std.mem.Allocator) void {
    allocator.free(list);
}

pub fn clone_results_list(list: []Inst, allocator: std.mem.Allocator) error{OutOfMemory}![]Inst {
    const new_list = try allocator.alloc(Inst, list.len);
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
/// For structs, takes ownership of the fields slice (caller must allocate fresh fields).
pub fn clobberInst(refinements: *Refinements, results: []Inst, index: usize, value: Refinement) !EIdx {
    const idx: EIdx = @intCast(refinements.list.items.len);
    try refinements.list.append(value);
    results[index].refinement = idx;
    return idx;
}

// =============================================================================
// Function lifecycle
// =============================================================================

pub fn onFinish(state: State) !void {
    try tag.splatFinish(state.results, state.ctx, state.refinements);
}

/// Recursively propagate pointee state from local to caller.
fn propagatePointee(local_refs: *Refinements, local_idx: EIdx, caller_refs: *Refinements, caller_idx: EIdx) void {
    const local = local_refs.at(local_idx);
    const caller = caller_refs.at(caller_idx);

    switch (local.*) {
        .scalar => |src| {
            // caller should already be scalar from type info
            if (caller.* != .scalar) std.debug.panic("backPropagate: local pointee is scalar but caller pointee is {s}", .{@tagName(caller.*)});
            caller.scalar = src;
        },
        .pointer => |src| {
            if (caller.* != .pointer) std.debug.panic("backPropagate: local pointee is pointer but caller pointee is {s}", .{@tagName(caller.*)});
            caller.pointer.analyte = src.analyte;
            propagatePointee(local_refs, src.to, caller_refs, caller.pointer.to);
        },
        .@"struct" => |src| {
            if (caller.* != .@"struct") std.debug.panic("backPropagate: local pointee is struct but caller pointee is {s}", .{@tagName(caller.*)});
            caller.@"struct".analyte = src.analyte;
            for (src.fields, 0..) |local_field_idx, i| {
                propagatePointee(local_refs, local_field_idx, caller_refs, caller.@"struct".fields[i]);
            }
        },
        .optional => |src| {
            caller.optional.analyte = src.analyte;
            propagatePointee(local_refs, src.to, caller_refs, caller.optional.to);
        },
        else => |t| std.debug.panic("backPropagate: local pointee is {s} - propagation not yet implemented", .{@tagName(t)}),
    }
}

/// Propagate analysis state back to callers via caller_ref.
/// Called after onFinish to copy state back to the caller's entity.
/// Propagates both:
/// - Pointer's analyte (memory_safety for allocation tracking)
/// - Pointee's analyte (undefined tracking for scalar values)
pub fn backPropagate(state: State) void {
    // Caller refinements are null at entrypoint - no caller to propagate analysis state to.
    // This is legitimate - entrypoint functions don't need backward propagation.
    const cp = state.caller_refinements orelse return;
    for (state.results) |inst| {
        // Skip non-arg instructions and interned args (compile-time constants)
        const caller_entity_idx = inst.caller_eidx orelse continue;
        const local_idx = inst.refinement orelse continue;

        const local_refinement = state.refinements.at(local_idx);
        const caller_entity = cp.at(caller_entity_idx);

        // Local and caller refinements have matching types - Arg copies structure from caller
        switch (local_refinement.*) {
            .scalar => |src| {
                caller_entity.scalar = src;
            },
            .pointer => |local_ptr| {
                caller_entity.pointer.analyte = local_ptr.analyte;
                propagatePointee(state.refinements, local_ptr.to, cp, caller_entity.pointer.to);
            },
            .optional => |local_opt| {
                caller_entity.optional.analyte = local_opt.analyte;
                propagatePointee(state.refinements, local_opt.to, cp, caller_entity.optional.to);
            },
            .@"struct" => |local_struct| {
                caller_entity.@"struct".analyte = local_struct.analyte;
                for (local_struct.fields, 0..) |local_field_idx, i| {
                    propagatePointee(state.refinements, local_field_idx, cp, caller_entity.@"struct".fields[i]);
                }
            },
            else => |t| std.debug.panic("backPropagate: local refinement is {s} - propagation not yet implemented", .{@tagName(t)}),
        }
    }
}

/// Assert all refinements are valid (non-null for all results that have one).
/// Accesses each refinement to trigger Zig's bounds checking if invalid.
pub fn assertAllValid(refinements: *Refinements, results: []const Inst) void {
    for (results) |inst| {
        if (inst.refinement) |idx| {
            _ = refinements.list.items[idx]; // Zig's bounds checking handles invalid indices
        }
    }
}

// =============================================================================
// Tests
// =============================================================================

/// Test helper to create a State for testing
fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_eidx = 0,
        .caller_refinements = null,
    };
}

test "alloc creates pointer to typed pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = try make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);
    try Inst.apply(state, 0, .{ .dbg_stmt = .{ .line = 0, .column = 0 } });
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // dbg_stmt sets instruction to void (no analysis tracking)
    try std.testing.expect(results[0].refinement != null);
    try std.testing.expectEqual(.void, std.meta.activeTag(results[0].get(&refinements).*));
    // alloc creates pointer; pointee type is determined by .ty parameter
    try std.testing.expect(results[1].refinement != null);
    try std.testing.expectEqual(.pointer, std.meta.activeTag(results[1].get(&refinements).*));
    const pointee_idx = results[1].get(&refinements).pointer.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(pointee_idx).*));
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

    const results = try make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .undefined = &.{ .scalar = {} } } } } });

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

    const results = try make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

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

    const results = try make_results_list(allocator, 4);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);

    // Set up: alloc creates pointer to future, store with .undefined transforms to scalar+undefined
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .undefined = &.{ .scalar = {} } } } } });

    // Load from undefined instruction should return error
    try std.testing.expectError(error.UseBeforeAssign, Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .scalar = {} } } }));
}

test "load from defined instruction does not report error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = try make_results_list(allocator, 4);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);

    // Set up: alloc then store a real value
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

    // Load from defined instruction should NOT return error
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .scalar = {} } } });
}

test "all instructions get valid refinements after operations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = try make_results_list(allocator, 5);
    defer clear_results_list(results, allocator);

    const state = testState(&ctx, results, &refinements);

    // Apply various operations that should all set refinements
    try Inst.apply(state, 0, .{ .dbg_stmt = .{ .line = 0, .column = 0 } });
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 4, .{ .block = .{ .ty = .{ .void = {} } } });

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
    const callee_results = try make_results_list(allocator, 3);
    defer clear_results_list(callee_results, allocator);

    // Caller's refinements - pre-allocate return entity
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    // Verify return entity is initially unset
    try std.testing.expectEqual(.retval_future, std.meta.activeTag(caller_refinements.at(return_eidx).*));

    // Create state with caller_refinements set
    const state = State{
        .ctx = &ctx,
        .results = callee_results,
        .refinements = &callee_refinements,
        .return_eidx = return_eidx,
        .caller_refinements = &caller_refinements,
    };

    // In callee: allocate and store a value
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .scalar = {} } } } });

    // Return the value from instruction 0
    try Inst.apply(state, 2, .{ .ret_safe = .{ .src = .{ .eidx = 0 } } });

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
    const callee_results = try make_results_list(allocator, 1);
    defer clear_results_list(callee_results, allocator);

    // Caller's refinements - pre-allocate return entity
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    // Create state with caller_refinements set
    const state = State{
        .ctx = &ctx,
        .results = callee_results,
        .refinements = &callee_refinements,
        .return_eidx = return_eidx,
        .caller_refinements = &caller_refinements,
    };

    // Return void
    try Inst.apply(state, 0, .{ .ret_safe = .{ .src = .{ .interned = .{ .void = {} } } } });

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
    const results = try make_results_list(allocator, 2);
    defer clear_results_list(results, allocator);

    // State with null caller_refinements (entrypoint)
    const state = testState(&ctx, results, &refinements);

    // Allocate a value
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .scalar = {} } } } });

    // Return with null caller_refinements (entrypoint case) - should just succeed without error
    try Inst.apply(state, 1, .{ .ret_safe = .{ .src = .{ .eidx = 0 } } });
}

// =============================================================================
// backPropagate Tests
// =============================================================================

test "backPropagate with null caller_refinements does nothing" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);

    // State with null caller_refinements
    const state = testState(&ctx, results, &refinements);

    // Should just return without error
    backPropagate(state);
}

test "backPropagate propagates scalar analyte to caller" {
    const allocator = std.testing.allocator;
    const undefined_analysis = @import("analysis/undefined.zig");

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

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
    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_scalar_idx;
    results[0].caller_eidx = caller_scalar_idx;
    results[0].name = "arg";

    // Verify caller scalar is initially undefined tracking = null
    try std.testing.expectEqual(@as(?undefined_analysis.Undefined, null), caller_refinements.at(caller_scalar_idx).scalar.undefined);

    // Create state with caller_refinements
    const state = State{
        .ctx = &ctx,
        .results = results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // Propagate
    backPropagate(state);

    // Verify caller scalar now has defined state
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_scalar_idx).scalar.undefined.?));
}

test "backPropagate propagates pointer analyte to caller" {
    const allocator = std.testing.allocator;
    const memory_safety = @import("analysis/memory_safety.zig");

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

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
    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_ptr_idx;
    results[0].caller_eidx = caller_ptr_idx;
    results[0].name = "ptr";

    // Verify caller pointer has no memory_safety initially
    try std.testing.expectEqual(@as(?memory_safety.MemorySafety, null), caller_refinements.at(caller_ptr_idx).pointer.analyte.memory_safety);

    // Create state with caller_refinements
    const state = State{
        .ctx = &ctx,
        .results = results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // Propagate
    backPropagate(state);

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
    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_ptr_idx;
    results[0].caller_eidx = caller_ptr_idx;
    results[0].name = "ptr";

    // Verify caller's pointee is undefined initially
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_pointee_idx).scalar.undefined.?));

    // Propagate
    const state = State{
        .ctx = undefined,
        .results = results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };
    backPropagate(state);

    // Verify caller's pointee is now defined
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_pointee_idx).scalar.undefined.?));
}

test "backPropagate propagates struct field undefined state to caller" {
    const allocator = std.testing.allocator;

    // Set up caller's refinements: pointer → struct { field0: undefined, field1: undefined }
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const caller_field0_idx = try caller_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .undefined = .{
            .meta = .{ .function = "", .file = "", .line = 0 },
        } },
    } });
    const caller_field1_idx = try caller_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .undefined = .{
            .meta = .{ .function = "", .file = "", .line = 0 },
        } },
    } });
    const caller_fields = try allocator.alloc(EIdx, 2);
    caller_fields[0] = caller_field0_idx;
    caller_fields[1] = caller_field1_idx;
    const caller_struct_idx = try caller_refinements.appendEntity(.{ .@"struct" = .{
        .fields = caller_fields,
    } });
    const caller_ptr_idx = try caller_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{ .undefined = .{ .defined = {} } },
        .to = caller_struct_idx,
    } });

    // Set up callee's refinements: pointer → struct { field0: defined, field1: undefined }
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_field0_idx = try callee_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .defined = {} },
    } });
    const callee_field1_idx = try callee_refinements.appendEntity(.{ .scalar = .{
        .undefined = .{ .undefined = .{
            .meta = .{ .function = "", .file = "", .line = 0 },
        } },
    } });
    const callee_fields = try allocator.alloc(EIdx, 2);
    callee_fields[0] = callee_field0_idx;
    callee_fields[1] = callee_field1_idx;
    const callee_struct_idx = try callee_refinements.appendEntity(.{ .@"struct" = .{
        .fields = callee_fields,
    } });
    const callee_ptr_idx = try callee_refinements.appendEntity(.{ .pointer = .{
        .analyte = .{ .undefined = .{ .defined = {} } },
        .to = callee_struct_idx,
    } });

    // Set up results with argument info
    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);
    results[0].refinement = callee_ptr_idx;
    results[0].caller_eidx = caller_ptr_idx;
    results[0].name = "ptr";

    // Verify caller's fields are undefined initially
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_field0_idx).scalar.undefined.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_field1_idx).scalar.undefined.?));

    // Propagate
    const state = State{
        .ctx = undefined,
        .results = results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };
    backPropagate(state);

    // Verify caller's field0 is now defined, field1 is still undefined
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_field0_idx).scalar.undefined.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_field1_idx).scalar.undefined.?));
}

test "full flow: callee modifies struct field via pointer-to-pointer chain" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    // === CALLER SETUP ===
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const caller_results = try make_results_list(allocator, 3);
    defer clear_results_list(caller_results, allocator);

    // Caller: inst 1 = alloc (struct with 2 fields); inst 2 = store_safe with undefined struct
    const struct_ty: tag.Type = .{ .@"struct" = &.{ .{ .ty = .{ .scalar = {} } }, .{ .ty = .{ .scalar = {} } } } };
    const caller_state = testState(&ctx, caller_results, &caller_refinements);
    try Inst.apply(caller_state, 1, .{ .alloc = .{ .ty = struct_ty } });
    try Inst.apply(caller_state, 2, .{ .store_safe = .{
        .ptr = 1,
        .src = .{ .interned = .{ .undefined = &struct_ty } },
    } });

    // Verify caller's struct has undefined fields
    const caller_ptr_idx = caller_results[1].refinement.?;
    const caller_struct_idx = caller_refinements.at(caller_ptr_idx).pointer.to;
    const caller_fields = caller_refinements.at(caller_struct_idx).@"struct".fields;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_fields[0]).scalar.undefined.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_fields[1]).scalar.undefined.?));

    // === CALLEE SETUP ===
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();
    const callee_results = try make_results_list(allocator, 8);
    defer clear_results_list(callee_results, allocator);

    const callee_state = State{
        .ctx = &ctx,
        .results = callee_results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // Callee: inst 0 = arg (pointer to struct)
    try Inst.apply(callee_state, 0, .{ .arg = .{ .value = .{ .eidx = caller_ptr_idx }, .name = "p" } });

    // Callee: inst 1 = alloc (pointer to struct), inst 2 = store inst 0's pointer to inst 1
    try Inst.apply(callee_state, 1, .{ .alloc = .{ .ty = .{ .pointer = &struct_ty } } });
    try Inst.apply(callee_state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .eidx = 0 } } });

    // Callee: inst 3 = bitcast inst 1 (shares refinement)
    try Inst.apply(callee_state, 3, .{ .bitcast = .{ .src = .{ .eidx = 1 } } });

    // Callee: inst 5 = load from inst 3 (gets the pointer stored in inst 1)
    try Inst.apply(callee_state, 5, .{ .load = .{ .ptr = 3, .ty = .{ .pointer = &.{ .@"struct" = &.{ .{ .ty = .{ .scalar = {} } }, .{ .ty = .{ .scalar = {} } } } } } } });

    // Callee: inst 6 = struct_field_ptr of field 0
    try Inst.apply(callee_state, 6, .{ .struct_field_ptr = .{
        .base = 5,
        .field_index = 0,
        .ty = .{ .pointer = &.{ .scalar = {} } },
    } });

    // Callee: inst 7 = store_safe to inst 6 (sets field0 to defined)
    try Inst.apply(callee_state, 7, .{ .store_safe = .{ .ptr = 6, .src = .{ .interned = .{ .scalar = {} } } } });

    // Check that callee's local field0 is now defined
    const local_ptr_idx = callee_results[0].refinement.?;
    const local_struct_idx = callee_refinements.at(local_ptr_idx).pointer.to;
    const local_fields = callee_refinements.at(local_struct_idx).@"struct".fields;
    try std.testing.expectEqual(.defined, std.meta.activeTag(callee_refinements.at(local_fields[0]).scalar.undefined.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(callee_refinements.at(local_fields[1]).scalar.undefined.?));

    // === BACKPROPAGATE ===
    backPropagate(callee_state);

    // After backPropagate, caller's field0 should be defined, field1 should be undefined
    try std.testing.expectEqual(.defined, std.meta.activeTag(caller_refinements.at(caller_fields[0]).scalar.undefined.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(caller_refinements.at(caller_fields[1]).scalar.undefined.?));
}
