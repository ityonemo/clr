const std = @import("std");
const codegen = @import("codegen.zig");
const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
const InternPool = compiler.InternPool;
const Tag = Air.Inst.Tag;
const Data = Air.Inst.Data;

// Test AllocatorVTable that wraps page_allocator for unit testing.
// We use page_allocator instead of testing.allocator because:
// 1. Tests may run in parallel with shared global allocator state
// 2. The clr_allocator manages its own arena lifetime
const page_allocator = std.heap.page_allocator;

fn testAlloc(_: ?[*]u8, len: usize, alignment: usize, ret_addr: usize) callconv(.c) ?[*]u8 {
    return @ptrCast(page_allocator.rawAlloc(len, @enumFromInt(alignment), ret_addr));
}

fn testResize(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, new_len: usize, ret_addr: usize) callconv(.c) bool {
    return page_allocator.rawResize(ptr.?[0..len], @enumFromInt(alignment), new_len, ret_addr);
}

fn testRemap(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, new_len: usize, ret_addr: usize) callconv(.c) ?[*]u8 {
    return page_allocator.rawRemap(ptr.?[0..len], @enumFromInt(alignment), new_len, ret_addr);
}

fn testFree(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, ret_addr: usize) callconv(.c) void {
    return page_allocator.rawFree(ptr.?[0..len], @enumFromInt(alignment), ret_addr);
}

const test_avt: clr_allocator.AllocatorVTable = .{
    .alloc = testAlloc,
    .resize = testResize,
    .remap = testRemap,
    .free = testFree,
};

var test_allocator_initialized: std.atomic.Value(bool) = .init(false);

fn initTestAllocator() void {
    // Use compare-exchange for thread-safe initialization
    if (test_allocator_initialized.cmpxchgStrong(false, true, .acquire, .monotonic) == null) {
        clr_allocator.init(&test_avt);
    }
}

fn deinitTestAllocator() void {
    // No-op: Don't deinit since tests may run in parallel sharing the allocator.
    // Memory will be reclaimed when the test process exits.
}

// Dummy InternPool pointer for tests that don't need it
const dummy_ip: *const InternPool = @ptrFromInt(0x1000);

test "instLine for dbg_stmt" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .dbg_stmt = .{ .line = 10, .column = 5 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .dbg_stmt, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .dbg_stmt = .{ .line = 10, .column = 5 } });\n", result);
}

test "instLine for arg" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum, 0, &.{}, &.{}, &.{}, &.{"test_param"}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name = \"test_param\" } });\n", result);
}

test "instLine for arg with sequential zir_param_index uses arg counter" {
    // Normal case: zir_param_index values are sequential (0, 1, 2).
    // The arg_counter should track these correctly.
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const param_names = &[_][]const u8{ "a", "b", "c" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name = \"a\" } });\n", result0);

    // Second arg: zir_param_index=1, should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 1 } };
    const result1 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 1, .{ .arg = .{ .value = arg1, .name = \"b\" } });\n", result1);

    // Third arg: zir_param_index=2, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result2 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .arg = .{ .value = arg2, .name = \"c\" } });\n", result2);

    try std.testing.expectEqual(@as(u32, 3), arg_counter);
}

test "instLine for arg with non-sequential zir_param_index uses sequential arg counter" {
    // When comptime params are present, zir_param_index can have gaps (e.g., 0, 2, 3).
    // The arg_counter ensures we generate sequential arg references (arg0, arg1, arg2)
    // while still using zir_param_index for name lookup.
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // Simulate 3 runtime args with zir_param_index 0, 2, 3 (gap at 1 = comptime param)
    const param_names = &[_][]const u8{ "self", "comptime_skip", "byte_count", "return_address" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name = \"self\" } });\n", result0);

    // Second arg: zir_param_index=2 (skipped 1), should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result1 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 1, .{ .arg = .{ .value = arg1, .name = \"byte_count\" } });\n", result1);

    // Third arg: zir_param_index=3, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 3 } };
    const result2 = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .arg = .{ .value = arg2, .name = \"return_address\" } });\n", result2);

    // Counter should be at 3 after processing 3 args
    try std.testing.expectEqual(@as(u32, 3), arg_counter);
}

test "instLine for ret_safe with source" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .ret_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .ret_safe = .{ .src = .{ .eidx = 5 } } });\n", result);
}

test "instLine for alloc" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // alloc uses datum.ty which is a pointer type - use well-known manyptr_u8_type
    const datum: Data = .{ .ty = .{ .ip_index = .manyptr_u8_type } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .alloc, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });\n", result);
}

test "instLine for store_safe" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const ptr_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const val_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .bin_op = .{ .lhs = ptr_ref, .rhs = val_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .store_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 3, .src = .{ .eidx = 4 } } });\n", result);
}

test "instLine for load" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .u8_type, .operand = operand_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .load, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .load = .{ .ptr = 5, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });\n", result);
}

test "generateFunction produces complete function" {
    initTestAllocator();
    defer deinitTestAllocator();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const tags: []const Tag = &.{ .alloc, .dbg_stmt, .load, .ret_safe };
    const load_ref: Ref = @enumFromInt(@as(u32, 0) | (1 << 31));
    const ret_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    const data: []const Data = &.{
        .{ .ty = .{ .ip_index = .manyptr_u8_type } }, // alloc needs pointer type
        .{ .dbg_stmt = .{ .line = 1, .column = 3 } },
        .{ .ty_op = .{ .ty = .u8_type, .operand = load_ref } },
        .{ .un_op = ret_ref },
    };
    // extra array format: extra[0] = block_index, extra[block_index] = body_len,
    // extra[block_index+1..] = body instruction indices
    // For 4 instructions (0,1,2,3) all in main body: block_index=1, body_len=4, indices=[0,1,2,3]
    const extra: []const u32 = &.{ 1, 4, 0, 1, 2, 3 };
    const result = codegen.generateFunction(42, "test.main", dummy_ip, tags, data, extra, 10, "test.zig", &.{}, &name_map);

    const expected =
        \\fn fn_42(ctx: *Context, caller_refinements: ?*Refinements) anyerror!EIdx {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\
        \\    var refinements = Refinements.init(ctx.allocator);
        \\    defer refinements.deinit();
        \\    defer refinements.testValid();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 4);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{ .retval_future = {} }) else 0;
        \\
        \\    const state = State{ .ctx = ctx, .results = results, .refinements = &refinements, .return_eidx = return_eidx, .caller_refinements = caller_refinements };
        \\
        \\    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 1, .{ .dbg_stmt = .{ .line = 1, .column = 3 } });
        \\    try Inst.apply(state, 2, .{ .load = .{ .ptr = 0, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .eidx = 2 } } });
        \\    try Inst.onFinish(state);
        \\    Inst.backPropagate(state);
        \\    return return_eidx;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "epilogue generates correct output" {
    initTestAllocator();
    defer deinitTestAllocator();

    const result = codegen.epilogue(123, false);

    const expected =
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Inst = clr.Inst;
        \\const Refinements = clr.Refinements;
        \\const EIdx = clr.EIdx;
        \\const Arg = clr.Arg;
        \\const State = clr.State;
        \\
        \\var writer_buf: [4096]u8 = undefined;
        \\var file_writer: std.fs.File.Writer = undefined;
        \\
        \\pub fn main() void {
        \\    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        \\    defer _ = gpa.deinit();
        \\    const allocator = gpa.allocator();
        \\    file_writer = std.fs.File.stdout().writer(&writer_buf);
        \\    defer file_writer.interface.flush() catch {};
        \\    var ctx = Context.init(allocator, &file_writer.interface);
        \\    defer ctx.deinit();
        \\    _ = fn_123(&ctx, null) catch {
        \\        file_writer.interface.flush() catch {};
        \\        std.process.exit(1);
        \\    };
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

// =============================================================================
// FQN Detection Tests
// =============================================================================

test "isDebugFqn matches debug.* patterns" {
    try std.testing.expect(codegen.isDebugFqn("debug.print"));
    try std.testing.expect(codegen.isDebugFqn("debug.assert"));
    try std.testing.expect(!codegen.isDebugFqn("std.debug.print"));
    try std.testing.expect(!codegen.isDebugFqn("mem.Allocator.create"));
}

test "isAllocatorCreateFqn matches Allocator.create patterns" {
    // Patterns from investigation
    try std.testing.expect(codegen.isAllocatorCreateFqn("mem.Allocator.create__anon_3464"));
    try std.testing.expect(codegen.isAllocatorCreateFqn("std.mem.Allocator.create__anon_1234"));
    // Should not match destroy
    try std.testing.expect(!codegen.isAllocatorCreateFqn("mem.Allocator.destroy__anon_3470"));
    // Should not match other functions
    try std.testing.expect(!codegen.isAllocatorCreateFqn("debug.print"));
    try std.testing.expect(!codegen.isAllocatorCreateFqn("foo.create"));
}

test "isAllocatorDestroyFqn matches Allocator.destroy patterns" {
    // Patterns from investigation
    try std.testing.expect(codegen.isAllocatorDestroyFqn("mem.Allocator.destroy__anon_3470"));
    try std.testing.expect(codegen.isAllocatorDestroyFqn("std.mem.Allocator.destroy__anon_5678"));
    // Should not match create
    try std.testing.expect(!codegen.isAllocatorDestroyFqn("mem.Allocator.create__anon_3464"));
    // Should not match other functions
    try std.testing.expect(!codegen.isAllocatorDestroyFqn("debug.print"));
    try std.testing.expect(!codegen.isAllocatorDestroyFqn("foo.destroy"));
}

test "instLine for br with block and operand" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // br: block_inst = 3, operand = inst 8
    // Ref encoding: high bit set = instruction index, lower 31 bits = index
    const operand_ref: Air.Inst.Ref = @enumFromInt((1 << 31) | 8);
    const datum: Data = .{ .br = .{
        .block_inst = @enumFromInt(3),
        .operand = operand_ref,
    } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .br, datum, 9, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 9, .{ .br = .{ .block = 3, .src = .{ .eidx = 8 } } });\n", result);
}

// =============================================================================
// TransferOp Tests (bitcast, unwrap_errunion_payload, optional_payload)
// =============================================================================

test "instLine for bitcast" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 7) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .bitcast, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .bitcast = .{ .src = .{ .eidx = 7 }, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });\n", result);
}

test "instLine for unwrap_errunion_payload" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .unwrap_errunion_payload, datum, 5, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 4 } } });\n", result);
}

test "instLine for optional_payload" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .optional_payload, datum, 6, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .optional_payload = .{ .src = .{ .eidx = 3 } } });\n", result);
}

// =============================================================================
// DbgVar Tests (dbg_var_ptr, dbg_var_val, dbg_arg_inline)
// =============================================================================

fn makeExtraWithString(comptime s: []const u8) []const u32 {
    // Pack string into u32 array with null terminator
    const len_with_null = s.len + 1;
    const word_count = (len_with_null + 3) / 4;
    var result: [word_count]u32 = [_]u32{0} ** word_count;
    const bytes: [*]u8 = @ptrCast(&result);
    @memcpy(bytes[0..s.len], s);
    // bytes[s.len] is already 0 from zero-init
    return &result;
}

test "instLine for dbg_var_ptr" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    // pl_op: operand is ptr, payload is index into extra for name string
    const extra = makeExtraWithString("my_var");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .dbg_var_ptr, datum, 3, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .dbg_var_ptr = .{ .ptr = 2, .name = \"my_var\" } });\n", result);
}

test "instLine for dbg_var_val" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const extra = makeExtraWithString("value_var");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .dbg_var_val, datum, 6, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .dbg_var_val = .{ .ptr = 5, .name = \"value_var\" } });\n", result);
}

test "instLine for dbg_arg_inline" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 1) | (1 << 31));
    const extra = makeExtraWithString("arg_name");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .dbg_arg_inline, datum, 2, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .dbg_arg_inline = .{ .ptr = 1, .name = \"arg_name\" } });\n", result);
}

// =============================================================================
// Conditional Branch Tests
// =============================================================================

// =============================================================================
// Missing Tag Tests - Simple/Unimplemented tags that emit .{}
// =============================================================================

test "instLine for unreach" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .no_op = {} };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .unreach, datum, 7, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 7, .{ .unreach = .{} });\n", result);
}

test "instLine for bit_and (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .bit_and, datum, 3, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .bit_and = .{} });\n", result);
}

test "instLine for cmp_eq (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .cmp_eq, datum, 4, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_eq = .{} });\n", result);
}

test "instLine for cmp_gt (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .cmp_gt, datum, 4, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_gt = .{} });\n", result);
}

test "instLine for cmp_lte (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .cmp_lte, datum, 4, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_lte = .{} });\n", result);
}

test "instLine for ctz (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .ctz, datum, 2, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .ctz = .{} });\n", result);
}

test "instLine for sub (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .sub, datum, 5, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .sub = .{} });\n", result);
}

test "instLine for is_non_err (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .un_op = .none };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .is_non_err, datum, 6, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .is_non_err = .{} });\n", result);
}

test "instLine for unwrap_errunion_err (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = .none } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .unwrap_errunion_err, datum, 8, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 8, .{ .unwrap_errunion_err = .{} });\n", result);
}

test "instLine for add_with_overflow (OverflowOp)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_pl = .{ .ty = .none, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .add_with_overflow, datum, 9, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 9, .{ .add_with_overflow = .{} });\n", result);
}

test "instLine for sub_with_overflow (OverflowOp)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_pl = .{ .ty = .none, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .sub_with_overflow, datum, 10, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 10, .{ .sub_with_overflow = .{} });\n", result);
}

// =============================================================================
// Missing Tag Tests - is_non_null, is_null (UnOp payload)
// =============================================================================

test "instLine for is_non_null" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .is_non_null, datum, 5, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .is_non_null = .{ .src = .{ .eidx = 4 } } });\n", result);
}

test "instLine for is_null" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .is_null, datum, 4, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .is_null = .{ .src = .{ .eidx = 3 } } });\n", result);
}

// =============================================================================
// Missing Tag Tests - ret_ptr, ret_load
// =============================================================================

test "instLine for ret_load" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const ptr_ref: Ref = @enumFromInt(@as(u32, 0) | (1 << 31));
    const datum: Data = .{ .un_op = ptr_ref };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .ret_load, datum, 5, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .ret_load = .{ .ptr = 0 } });\n", result);
}

// =============================================================================
// Missing Tag Tests - struct_field_ptr, struct_field_val
// =============================================================================

test "instLine for struct_field_ptr_index_0" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const base_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    // struct_field_ptr uses ty_op: ty is result type (pointer to field), operand is base
    // Use .u8_type for a valid type reference
    const datum: Data = .{ .ty_op = .{ .ty = .u8_type, .operand = base_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .struct_field_ptr_index_0, datum, 3, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .struct_field_ptr = .{ .base = 2, .field_index = 0, .field_name_id = 0, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });\n", result);
}

test "instLine for struct_field_ptr_index_1" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const base_ref: Ref = @enumFromInt(@as(u32, 1) | (1 << 31));
    // Use .u8_type for a valid type reference
    const datum: Data = .{ .ty_op = .{ .ty = .u8_type, .operand = base_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .struct_field_ptr_index_1, datum, 4, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .struct_field_ptr = .{ .base = 1, .field_index = 1, .field_name_id = 0, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });\n", result);
}

test "instLine for get_union_tag" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .get_union_tag, datum, 6, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .get_union_tag = .{ .operand = 5 } });\n", result);
}

test "instLine for block" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // block uses ty_pl: ty is the block's result type
    const datum: Data = .{ .ty_pl = .{ .ty = .void_type, .payload = 0 } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .block, datum, 2, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .id = 0, .ty = .{ .void = {} } } } });\n", result);
}

test "instLine for store (same as store_safe)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const ptr_ref: Ref = @enumFromInt(@as(u32, 1) | (1 << 31));
    const val_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    const datum: Data = .{ .bin_op = .{ .lhs = ptr_ref, .rhs = val_ref } };
    const result = codegen._instLine(&name_map, arena.allocator(), dummy_ip, .store, datum, 3, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .store = .{ .ptr = 1, .src = .{ .eidx = 2 } } });\n", result);
}

// =============================================================================
// Conditional Branch Tests
// =============================================================================

test "generateFunction with simple cond_br block" {
    // Test a simple if statement:
    //   var x: u8 = undefined;
    //   if (cond) { x = 5; }
    //   return x;
    //
    // AIR structure:
    //   0: alloc              <- x
    //   1: store_safe         <- x = undefined
    //   2: block              <- if block, body_len=5, body=[3,4,5,6,7]
    //   3: load (condition)
    //   4: store_safe (x = 5) <- then body
    //   5: br                 <- then body
    //   6: br                 <- else body
    //   7: cond_br
    //   8: load               <- load x
    //   9: ret_safe           <- return x

    initTestAllocator();
    defer deinitTestAllocator();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;

    // Build instruction arrays
    const tags: []const Tag = &.{
        .alloc, // 0: alloc for x
        .store_safe, // 1: store undef to x
        .block, // 2: if block
        .load, // 3: load condition (inside block body)
        .store_safe, // 4: store 5 to x (then branch)
        .br, // 5: br block (then branch)
        .br, // 6: br block (else branch)
        .cond_br, // 7: cond_br
        .load, // 8: load x (after block)
        .ret_safe, // 9: return
    };

    // Build refs for instructions
    const ref_0: Ref = @enumFromInt(@as(u32, 0) | (1 << 31)); // inst 0
    const ref_3: Ref = @enumFromInt(@as(u32, 3) | (1 << 31)); // inst 3
    const ref_8: Ref = @enumFromInt(@as(u32, 8) | (1 << 31)); // inst 8

    // Build data array
    const data: []const Data = &.{
        .{ .ty = .{ .ip_index = .manyptr_u8_type } }, // 0: alloc
        .{ .bin_op = .{ .lhs = ref_0, .rhs = .undef } }, // 1: store undef to x
        .{ .ty_pl = .{ .ty = .none, .payload = 7 } }, // 2: block (payload=7 points to block body in extra)
        .{ .ty_op = .{ .ty = .u8_type, .operand = .none } }, // 3: load condition
        .{ .bin_op = .{ .lhs = ref_0, .rhs = ref_3 } }, // 4: store to x (then)
        .{ .br = .{ .block_inst = @enumFromInt(2), .operand = .none } }, // 5: br block
        .{ .br = .{ .block_inst = @enumFromInt(2), .operand = .none } }, // 6: br block
        .{ .pl_op = .{ .operand = ref_3, .payload = 10 } }, // 7: cond_br (payload=10 points to extra[10])
        .{ .ty_op = .{ .ty = .u8_type, .operand = ref_0 } }, // 8: load x
        .{ .un_op = ref_8 }, // 9: ret_safe
    };

    // Build extra array
    // The function's MAIN body is the top-level instructions [0, 1, 2, 8, 9].
    // Instructions 3 and 7 are directly in block 2's body.
    // Instructions 4, 5, 6 are inside cond_br 7's branches.
    //
    // extra[0] = block_index = 1 (where main body Block structure starts)
    // Main body at extra[1]:
    //   extra[1] = body_len = 5
    //   extra[2..7] = body indices = [0, 1, 2, 8, 9] (top-level instructions)
    // Block 2's body at extra[7]:
    //   extra[7] = body_len = 2
    //   extra[8..10] = body indices = [3, 7] (load condition, cond_br)
    // CondBr at extra[10]:
    //   extra[10] = then_body_len = 2
    //   extra[11] = else_body_len = 1
    //   extra[12] = branch_hints = 0
    //   extra[13..15] = then_body = [4, 5]
    //   extra[15] = else_body = [6]
    const extra: []const u32 = &.{
        1, // block_index for main body
        5, // body_len for main body
        0, 1, 2, 8, 9, // main body indices (top-level)
        2, // block 2's body_len
        3, 7, // block 2's body indices (load, cond_br)
        2, // then_body_len
        1, // else_body_len
        0, // branch_hints
        4, 5, // then body indices
        6, // else body indices
    };

    const result = codegen.generateFunction(42, "test.main", dummy_ip, tags, data, extra, 10, "test.zig", &.{}, &name_map);

    // Expected output:
    // 1. Branch functions should be generated for true/false branches (named by cond_br index)
    // 2. Main function includes block instruction and condition load
    // 3. cond_br branches (instructions 4,5,6) are in sub functions only

    const expected =
        \\fn fn_42_cond_br_false_7(state: State) anyerror!void {
        \\    try Inst.apply(state, 0, .{ .cond_br = .{ .branch = false, .condition_idx = 3 } });
        \\    try Inst.apply(state, 6, .{ .br = .{ .block = 2, .src = .{ .interned = .{ .id = 0, .ty = .{ .void = {} } } } } });
        \\}
        \\
        \\fn fn_42_cond_br_true_7(state: State) anyerror!void {
        \\    try Inst.apply(state, 0, .{ .cond_br = .{ .branch = true, .condition_idx = 3 } });
        \\    try Inst.apply(state, 4, .{ .store_safe = .{ .ptr = 0, .src = .{ .eidx = 3 } } });
        \\    try Inst.apply(state, 5, .{ .br = .{ .block = 2, .src = .{ .interned = .{ .id = 0, .ty = .{ .void = {} } } } } });
        \\}
        \\
        \\fn fn_42(ctx: *Context, caller_refinements: ?*Refinements) anyerror!EIdx {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\
        \\    var refinements = Refinements.init(ctx.allocator);
        \\    defer refinements.deinit();
        \\    defer refinements.testValid();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 10);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{ .retval_future = {} }) else 0;
        \\
        \\    const state = State{ .ctx = ctx, .results = results, .refinements = &refinements, .return_eidx = return_eidx, .caller_refinements = caller_refinements };
        \\
        \\    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .id = 0, .ty = .{ .undefined = &.{ .id = 0, .ty = .{ .scalar = {} } } } } } } });
        \\    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .id = 0, .ty = .{ .void = {} } } } });
        \\    try Inst.apply(state, 3, .{ .load = .{ .ptr = null, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 4, .{ .noop = .{} });
        \\    try Inst.apply(state, 5, .{ .noop = .{} });
        \\    try Inst.apply(state, 6, .{ .noop = .{} });
        \\    try Inst.cond_br(state, 7, fn_42_cond_br_true_7, fn_42_cond_br_false_7);
        \\    try Inst.apply(state, 8, .{ .load = .{ .ptr = 0, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 9, .{ .ret_safe = .{ .src = .{ .eidx = 8 } } });
        \\    try Inst.onFinish(state);
        \\    Inst.backPropagate(state);
        \\    return return_eidx;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

