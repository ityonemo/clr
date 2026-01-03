const std = @import("std");
const tag = @import("tag.zig");
const Context = @import("Context.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const Analyte = Refinements.Analyte;
pub const Gid = Refinements.Gid;
pub const State = @import("lib.zig").State;

const Inst = @This();

/// GID into the Refinements entity table, or null if not yet initialized.
refinement: ?Gid = null,

/// The tag that created this instruction, stored for path building at error time.
/// Used by Context.buildPathName to walk the instruction chain statelessly.
inst_tag: ?tag.AnyTag = null,

/// Root variable name ID, set by dbg_var_ptr/dbg_var_val.
/// Separate from inst_tag because dbg_var_ptr names a different instruction.
/// Resolved via ctx.getName() at error time.
name_id: ?u32 = null,

/// Get this instruction's Refinement. Crashes if not initialized.
pub fn get(self: *Inst, refinements: *Refinements) *Refinement {
    return refinements.at(self.refinement.?);
}

pub fn apply(state: State, index: usize, any_tag: tag.AnyTag) !void {
    state.results[index].inst_tag = any_tag;
    switch (any_tag) {
        inline else => |t| try t.apply(state, index),
    }
}

pub fn call(state: State, index: usize, called: anytype, return_type: tag.Type, args: anytype) !void {
    // Skip if called is null (indirect call through function pointer - TODO: handle these)
    if (@TypeOf(called) == @TypeOf(null)) return;
    // Save caller's base_line - callee will set its own
    const saved_base_line = state.ctx.base_line;
    // Create typed return slot in global refinements table
    const return_ref = try tag.typeToRefinement(return_type, state.refinements);
    const return_slot = try state.refinements.appendEntity(return_ref);
    tag.splatInit(state.refinements, return_slot, state.ctx);
    // Call function with ctx, refinements, and return_slot
    const return_gid = try @call(.auto, called, .{ state.ctx, state.refinements, return_slot } ++ args);
    // Restore caller's base_line
    state.ctx.base_line = saved_base_line;
    // Clear "returned" flag on any allocations in the return value.
    // The callee marked them as "returned" (its responsibility ends),
    // but the caller now owns them and must either free or return them.
    const MemorySafety = @import("analysis/memory_safety.zig").MemorySafety;
    MemorySafety.clearAllocationsReturned(state.refinements, return_gid);
    // Deposit returned entity GID into caller's instruction
    state.results[index].refinement = return_gid;
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
    const return_gid = state.return_gid;

    // Clone results and refinements for each branch
    const true_results = try clone_results_list(results, ctx.allocator);
    defer clear_results_list(true_results, ctx.allocator);
    var true_refinements = try state.refinements.clone(ctx.allocator);
    defer true_refinements.deinit();

    const false_results = try clone_results_list(results, ctx.allocator);
    defer clear_results_list(false_results, ctx.allocator);
    var false_refinements = try state.refinements.clone(ctx.allocator);
    defer false_refinements.deinit();

    // Create GID tracking lists for each branch
    var true_created = std.ArrayListUnmanaged(Refinements.Gid){};
    defer true_created.deinit(ctx.allocator);
    var true_modified = std.ArrayListUnmanaged(Refinements.Gid){};
    defer true_modified.deinit(ctx.allocator);

    var false_created = std.ArrayListUnmanaged(Refinements.Gid){};
    defer false_created.deinit(ctx.allocator);
    var false_modified = std.ArrayListUnmanaged(Refinements.Gid){};
    defer false_modified.deinit(ctx.allocator);

    // Track if each branch returns (for merge exclusion)
    var true_returns: bool = false;
    var false_returns: bool = false;

    // Build state for each branch - same ctx, cloned refinements
    const true_state = State{
        .ctx = ctx,
        .results = true_results,
        .refinements = &true_refinements,
        .return_gid = return_gid,
        .created_gids = &true_created,
        .modified_gids = &true_modified,
        .branch_returns = &true_returns,
    };
    const false_state = State{
        .ctx = ctx,
        .results = false_results,
        .refinements = &false_refinements,
        .return_gid = return_gid,
        .created_gids = &false_created,
        .modified_gids = &false_modified,
        .branch_returns = &false_returns,
    };

    // Save ctx.meta before branches execute (they modify it via dbg_stmt)
    // The merge should use the meta at the cond_br, not inside a branch
    const saved_meta = ctx.meta;

    // Execute both branches (they modify their cloned state)
    try true_fn(true_state);
    try false_fn(false_state);

    // Restore ctx.meta for the merge - errors should point to the cond_br location
    ctx.meta = saved_meta;

    // Mark the block instruction as void
    results[index].refinement = try state.refinements.appendEntity(.{ .void = {} });

    // Merge: walk results and call analysis merge for each slot that has refinements
    const branches = [_]State{ true_state, false_state };
    try tag.splatMerge(.cond_br, results, ctx, state.refinements, &branches);

    // Merge return values from returning branches (hoisting to function level)
    try tag.mergeReturnValue(.cond_br, ctx.allocator, ctx, state.refinements, return_gid, &branches);

    // Propagate created/modified GIDs to parent's lists if tracking
    if (state.created_gids) |parent_created| {
        try parent_created.appendSlice(ctx.allocator, true_created.items);
        try parent_created.appendSlice(ctx.allocator, false_created.items);
    }
    if (state.modified_gids) |parent_modified| {
        try parent_modified.appendSlice(ctx.allocator, true_modified.items);
        try parent_modified.appendSlice(ctx.allocator, false_modified.items);
    }
}

/// Handle switch statement with N case branches plus optional else.
/// All branches are executed to get conservative analysis results.
/// Uses comptime tuple of function pointers for the case functions.
pub fn switch_br(
    state: State,
    comptime index: usize,
    comptime case_fns: anytype,
) !void {
    const ctx = state.ctx;
    const results = state.results;
    const return_gid = state.return_gid;

    const num_cases = case_fns.len;

    // Create arrays for each branch's cloned state
    var branch_results: [num_cases][]Inst = undefined;
    var branch_refinements: [num_cases]Refinements = undefined;
    var branch_created: [num_cases]std.ArrayListUnmanaged(Refinements.Gid) = undefined;
    var branch_modified: [num_cases]std.ArrayListUnmanaged(Refinements.Gid) = undefined;
    var branch_returns: [num_cases]bool = [_]bool{false} ** num_cases;

    // Clone state for each branch - same ctx, cloned refinements
    inline for (0..num_cases) |i| {
        branch_results[i] = try clone_results_list(results, ctx.allocator);
        branch_refinements[i] = try state.refinements.clone(ctx.allocator);
        branch_created[i] = .{};
        branch_modified[i] = .{};
    }

    // Deferred cleanup
    defer {
        inline for (0..num_cases) |i| {
            clear_results_list(branch_results[i], ctx.allocator);
            branch_refinements[i].deinit();
            branch_created[i].deinit(ctx.allocator);
            branch_modified[i].deinit(ctx.allocator);
        }
    }

    // Build states and execute each branch
    var branch_states: [num_cases]State = undefined;
    inline for (0..num_cases) |i| {
        branch_states[i] = State{
            .ctx = ctx,
            .results = branch_results[i],
            .refinements = &branch_refinements[i],
            .return_gid = return_gid,
            .created_gids = &branch_created[i],
            .modified_gids = &branch_modified[i],
            .branch_returns = &branch_returns[i],
        };
    }

    // Save ctx.meta before branches execute (they modify it via dbg_stmt)
    // The merge should use the meta at the switch_br, not inside a branch
    const saved_meta = ctx.meta;

    // Execute all branches
    inline for (0..num_cases) |i| {
        try case_fns[i](branch_states[i]);
    }

    // Restore ctx.meta for the merge - errors should point to the switch_br location
    ctx.meta = saved_meta;

    // Mark the switch instruction as void
    results[index].refinement = try state.refinements.appendEntity(.{ .void = {} });

    // Merge all branches using splatMerge
    try tag.splatMerge(.switch_br, results, ctx, state.refinements, &branch_states);

    // Merge return values from returning branches (hoisting to function level)
    try tag.mergeReturnValue(.switch_br, ctx.allocator, ctx, state.refinements, return_gid, &branch_states);

    // Propagate created/modified GIDs to parent's lists if tracking
    if (state.created_gids) |parent_created| {
        inline for (0..num_cases) |i| {
            try parent_created.appendSlice(ctx.allocator, branch_created[i].items);
        }
    }
    if (state.modified_gids) |parent_modified| {
        inline for (0..num_cases) |i| {
            try parent_modified.appendSlice(ctx.allocator, branch_modified[i].items);
        }
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
pub fn initInst(refinements: *Refinements, results: []Inst, index: usize) !Gid {
    if (results[index].refinement) |_| @panic("instruction already initialized");
    const gid = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    results[index].refinement = gid;
    return gid;
}

/// Overwrite an instruction with a new refinement. Creates new entity regardless of prior state.
/// Call this in tag handlers before splat() so analyses can set their respective fields.
/// For conditional keep-of-previous-value semantics, use merge (not yet implemented).
/// For structs, takes ownership of the fields slice (caller must allocate fresh fields).
pub fn clobberInst(refinements: *Refinements, results: []Inst, index: usize, value: Refinement) !Gid {
    const gid = try refinements.appendEntity(value);
    results[index].refinement = gid;
    return gid;
}

// =============================================================================
// Function lifecycle
// =============================================================================

pub fn onFinish(state: State) !void {
    try tag.splatFinish(state.results, state.ctx, state.refinements);
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
        .return_gid = 0,
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .undefined = &.{ .id = null, .ty = .{ .scalar = {} } } } } } } });

    // alloc's pointee stays undefined after store_safe with undef
    const pointee_idx = results[1].get(&refinements).pointer.to;
    const scalar = &refinements.at(pointee_idx).scalar;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(scalar.analyte.undefined.?));
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // alloc's pointee becomes defined after store_safe with real value
    const pointee_idx = results[1].get(&refinements).pointer.to;
    const scalar = &refinements.at(pointee_idx).scalar;
    try std.testing.expectEqual(.defined, std.meta.activeTag(scalar.analyte.undefined.?));
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .undefined = &.{ .id = null, .ty = .{ .scalar = {} } } } } } } });

    // Load from undefined instruction should return error
    try std.testing.expectError(error.UseBeforeAssign, Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } }));
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Load from defined instruction should NOT return error
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 4, .{ .block = .{ .ty = .{ .id = null, .ty = .{ .void = {} } } } });

    // All instructions should have valid refinements
    assertAllValid(&refinements, results);
}

test "ret_safe writes return value to return slot" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func"); // ret_safe needs a function name on stacktrace

    // Global refinements table - return slot pre-allocated with typed slot
    var refinements = Refinements.init(allocator);
    defer refinements.deinit();
    const return_type: tag.Type = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } };
    const return_ref = try tag.typeToRefinement(return_type, &refinements);
    const return_gid = try refinements.appendEntity(return_ref);
    tag.splatInit(&refinements, return_gid, &ctx);

    const results = try make_results_list(allocator, 3);
    defer clear_results_list(results, allocator);

    // Verify return entity is initially a pointer (typed slot)
    try std.testing.expectEqual(.pointer, std.meta.activeTag(refinements.at(return_gid).*));

    // Create state with return_gid pointing to return slot
    const state = State{
        .ctx = &ctx,
        .results = results,
        .refinements = &refinements,
        .return_gid = return_gid,
    };

    // Allocate and store a value
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Return the value from instruction 0
    try Inst.apply(state, 2, .{ .ret_safe = .{ .src = .{ .inst = 0 } } });

    // Verify return entity is still a pointer (overwritten with return value)
    try std.testing.expectEqual(.pointer, std.meta.activeTag(refinements.at(return_gid).*));
}

test "ret_safe with void src sets return to void" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func");

    // Global refinements table - return slot pre-allocated with void type
    var refinements = Refinements.init(allocator);
    defer refinements.deinit();
    const return_gid = try refinements.appendEntity(.{ .void = {} });

    const results = try make_results_list(allocator, 1);
    defer clear_results_list(results, allocator);

    // Create state with return_gid
    const state = State{
        .ctx = &ctx,
        .results = results,
        .refinements = &refinements,
        .return_gid = return_gid,
    };

    // Return void
    try Inst.apply(state, 0, .{ .ret_safe = .{ .src = .{ .interned = .{ .id = null, .ty = .{ .void = {} } } } } });

    // Verify return entity is still void
    try std.testing.expectEqual(.void, std.meta.activeTag(refinements.at(return_gid).*));
}

test "ret_safe at entrypoint succeeds" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();
    try ctx.stacktrace.append(allocator, "test_func");

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    // Create typed return slot (like entrypoint would)
    const return_type: tag.Type = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } };
    const return_ref = try tag.typeToRefinement(return_type, &refinements);
    const return_gid = try refinements.appendEntity(return_ref);
    tag.splatInit(&refinements, return_gid, &ctx);

    const results = try make_results_list(allocator, 2);
    defer clear_results_list(results, allocator);

    const state = State{
        .ctx = &ctx,
        .results = results,
        .refinements = &refinements,
        .return_gid = return_gid,
    };

    // Allocate a value
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Return - should just succeed without error
    try Inst.apply(state, 1, .{ .ret_safe = .{ .src = .{ .inst = 0 } } });
}
