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

    const datum: Data = .{ .dbg_stmt = .{ .line = 10, .column = 5 } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .dbg_stmt, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .dbg_stmt = .{ .line = 10, .column = 5 } }, results, ctx, &refinements);\n", result);
}

test "instLine for arg" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum, 0, &.{}, &.{}, &.{}, &.{"test_param"}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .arg = .{ .value = arg0, .name = \"test_param\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result);
}

test "instLine for arg with sequential zir_param_index uses arg counter" {
    // Normal case: zir_param_index values are sequential (0, 1, 2).
    // The arg_counter should track these correctly.
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const param_names = &[_][]const u8{ "a", "b", "c" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .arg = .{ .value = arg0, .name = \"a\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result0);

    // Second arg: zir_param_index=1, should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 1 } };
    const result1 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(1, .{ .arg = .{ .value = arg1, .name = \"b\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result1);

    // Third arg: zir_param_index=2, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result2 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(2, .{ .arg = .{ .value = arg2, .name = \"c\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result2);

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

    // Simulate 3 runtime args with zir_param_index 0, 2, 3 (gap at 1 = comptime param)
    const param_names = &[_][]const u8{ "self", "comptime_skip", "byte_count", "return_address" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .arg = .{ .value = arg0, .name = \"self\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result0);

    // Second arg: zir_param_index=2 (skipped 1), should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result1 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(1, .{ .arg = .{ .value = arg1, .name = \"byte_count\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result1);

    // Third arg: zir_param_index=3, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 3 } };
    const result2 = codegen._instLine(arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Inst.apply(2, .{ .arg = .{ .value = arg2, .name = \"return_address\", .caller_refinements = caller_refinements } }, results, ctx, &refinements);\n", result2);

    // Counter should be at 3 after processing 3 args
    try std.testing.expectEqual(@as(u32, 3), arg_counter);
}

test "instLine for ret_safe with source" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .ret_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .ret_safe = .{ .caller_refinements = caller_refinements, .return_eidx = return_eidx, .src = .{ .eidx = 5 } } }, results, ctx, &refinements);\n", result);
}

test "instLine for alloc" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .no_op = {} };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .alloc, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .alloc = .{} }, results, ctx, &refinements);\n", result);
}

test "instLine for store_safe" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const ptr_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const val_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .bin_op = .{ .lhs = ptr_ref, .rhs = val_ref } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .store_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .store_safe = .{ .ptr = 3, .src = .{ .eidx = 4 }, .is_undef = false } }, results, ctx, &refinements);\n", result);
}

test "instLine for load" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .load, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .load = .{ .ptr = 5 } }, results, ctx, &refinements);\n", result);
}

test "generateFunction produces complete function" {
    initTestAllocator();
    defer deinitTestAllocator();

    const Ref = Air.Inst.Ref;
    const tags: []const Tag = &.{ .alloc, .dbg_stmt, .load, .ret_safe };
    const load_ref: Ref = @enumFromInt(@as(u32, 0) | (1 << 31));
    const ret_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    const data: []const Data = &.{
        .{ .no_op = {} },
        .{ .dbg_stmt = .{ .line = 1, .column = 3 } },
        .{ .ty_op = .{ .ty = .none, .operand = load_ref } },
        .{ .un_op = ret_ref },
    };
    const result = codegen.generateFunction(42, "test.main", dummy_ip, tags, data, &.{}, 10, "test.zig", &.{});

    const expected =
        \\fn fn_42(ctx: *Context, caller_refinements: ?*Refinements) anyerror!EIdx {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\
        \\    var refinements = Refinements.init(ctx.allocator);
        \\    defer refinements.deinit();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 4);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{ .retval_future = {} }) else 0;
        \\
        \\    try Inst.apply(0, .{ .alloc = .{} }, results, ctx, &refinements);
        \\    try Inst.apply(1, .{ .dbg_stmt = .{ .line = 1, .column = 3 } }, results, ctx, &refinements);
        \\    try Inst.apply(2, .{ .load = .{ .ptr = 0 } }, results, ctx, &refinements);
        \\    try Inst.apply(3, .{ .ret_safe = .{ .caller_refinements = caller_refinements, .return_eidx = return_eidx, .src = .{ .eidx = 2 } } }, results, ctx, &refinements);
        \\    try Inst.onFinish(results, ctx, &refinements);
        \\    Inst.backPropagate(results, &refinements, caller_refinements);
        \\    return return_eidx;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "epilogue generates correct output" {
    initTestAllocator();
    defer deinitTestAllocator();

    const result = codegen.epilogue(123);

    const expected =
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Inst = clr.Inst;
        \\const Refinements = clr.Refinements;
        \\const EIdx = clr.EIdx;
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

    // br: block_inst = 3, operand = inst 8
    // Ref encoding: high bit set = instruction index, lower 31 bits = index
    const operand_ref: Air.Inst.Ref = @enumFromInt((1 << 31) | 8);
    const datum: Data = .{ .br = .{
        .block_inst = @enumFromInt(3),
        .operand = operand_ref,
    } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .br, datum, 9, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(9, .{ .br = .{ .block = 3, .src = .{ .eidx = 8 } } }, results, ctx, &refinements);\n", result);
}

// =============================================================================
// TransferOp Tests (bitcast, unwrap_errunion_payload, optional_payload)
// =============================================================================

test "instLine for bitcast" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 7) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .bitcast, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .bitcast = .{ .src = .{ .eidx = 7 } } }, results, ctx, &refinements);\n", result);
}

test "instLine for unwrap_errunion_payload" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .unwrap_errunion_payload, datum, 5, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(5, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 4 } } }, results, ctx, &refinements);\n", result);
}

test "instLine for optional_payload" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .optional_payload, datum, 6, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(6, .{ .optional_payload = .{ .src = .{ .eidx = 3 } } }, results, ctx, &refinements);\n", result);
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

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 2) | (1 << 31));
    // pl_op: operand is ptr, payload is index into extra for name string
    const extra = makeExtraWithString("my_var");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .dbg_var_ptr, datum, 3, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(3, .{ .dbg_var_ptr = .{ .ptr = 2, .name = \"my_var\" } }, results, ctx, &refinements);\n", result);
}

test "instLine for dbg_var_val" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const extra = makeExtraWithString("value_var");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .dbg_var_val, datum, 6, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(6, .{ .dbg_var_val = .{ .ptr = 5, .name = \"value_var\" } }, results, ctx, &refinements);\n", result);
}

test "instLine for dbg_arg_inline" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 1) | (1 << 31));
    const extra = makeExtraWithString("arg_name");
    const datum: Data = .{ .pl_op = .{ .operand = operand_ref, .payload = 0 } };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .dbg_arg_inline, datum, 2, extra, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(2, .{ .dbg_arg_inline = .{ .ptr = 1, .name = \"arg_name\" } }, results, ctx, &refinements);\n", result);
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
        .{ .no_op = {} }, // 0: alloc
        .{ .bin_op = .{ .lhs = ref_0, .rhs = .undef } }, // 1: store undef to x
        .{ .ty_pl = .{ .ty = .none, .payload = 0 } }, // 2: block (payload=0 points to extra[0])
        .{ .ty_op = .{ .ty = .none, .operand = .none } }, // 3: load condition
        .{ .bin_op = .{ .lhs = ref_0, .rhs = ref_3 } }, // 4: store to x (then)
        .{ .br = .{ .block_inst = @enumFromInt(2), .operand = .none } }, // 5: br block
        .{ .br = .{ .block_inst = @enumFromInt(2), .operand = .none } }, // 6: br block
        .{ .pl_op = .{ .operand = ref_3, .payload = 6 } }, // 7: cond_br (payload=6 points to extra[6])
        .{ .ty_op = .{ .ty = .none, .operand = ref_0 } }, // 8: load x
        .{ .un_op = ref_8 }, // 9: ret_safe
    };

    // Build extra array
    // Block at extra[0]:
    //   extra[0] = body_len = 5
    //   extra[1..6] = body indices = [3, 4, 5, 6, 7]
    // CondBr at extra[6]:
    //   extra[6] = then_body_len = 2
    //   extra[7] = else_body_len = 1
    //   extra[8] = branch_hints = 0
    //   extra[9..11] = then_body = [4, 5]
    //   extra[11] = else_body = [6]
    const extra: []const u32 = &.{
        5, // body_len for block
        3, 4, 5, 6, 7, // body indices [3,4,5,6,7]
        2, // then_body_len
        1, // else_body_len
        0, // branch_hints
        4, 5, // then body indices
        6, // else body indices
    };

    const result = codegen.generateFunction(42, "test.main", dummy_ip, tags, data, extra, 10, "test.zig", &.{});

    // Expected output:
    // 1. Branch functions should be generated for true/false branches (named by cond_br index)
    // 2. Main function includes block instruction and condition load
    // 3. cond_br branches (instructions 4,5,6) are in sub functions only

    const expected =
        \\fn fn_42_cond_br_false_7(results: []Inst, ctx: *Context, refinements: *Refinements, caller_refinements: ?*Refinements, return_eidx: EIdx) anyerror!void {
        \\    _ = caller_refinements;
        \\    _ = return_eidx;
        \\    try Inst.apply(6, .{ .br = .{ .block = 2, .src = .{ .interned = .{ .void = {} } } } }, results, ctx, refinements);
        \\}
        \\fn fn_42_cond_br_true_7(results: []Inst, ctx: *Context, refinements: *Refinements, caller_refinements: ?*Refinements, return_eidx: EIdx) anyerror!void {
        \\    _ = caller_refinements;
        \\    _ = return_eidx;
        \\    try Inst.apply(4, .{ .store_safe = .{ .ptr = 0, .src = .{ .eidx = 3 }, .is_undef = false } }, results, ctx, refinements);
        \\    try Inst.apply(5, .{ .br = .{ .block = 2, .src = .{ .interned = .{ .void = {} } } } }, results, ctx, refinements);
        \\}
        \\fn fn_42(ctx: *Context, caller_refinements: ?*Refinements) anyerror!EIdx {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\
        \\    var refinements = Refinements.init(ctx.allocator);
        \\    defer refinements.deinit();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 10);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{ .retval_future = {} }) else 0;
        \\
        \\    try Inst.apply(0, .{ .alloc = .{} }, results, ctx, &refinements);
        \\    try Inst.apply(1, .{ .store_safe = .{ .ptr = 0, .src = .{ .interned = .{ .scalar = {} } }, .is_undef = true } }, results, ctx, &refinements);
        \\    try Inst.apply(2, .{ .block = .{} }, results, ctx, &refinements);
        \\    try Inst.apply(3, .{ .load = .{ .ptr = null } }, results, ctx, &refinements);
        \\    try Inst.cond_br(7, fn_42_cond_br_true_7, fn_42_cond_br_false_7, results, ctx, &refinements, caller_refinements, return_eidx);
        \\    try Inst.apply(8, .{ .load = .{ .ptr = 0 } }, results, ctx, &refinements);
        \\    try Inst.apply(9, .{ .ret_safe = .{ .caller_refinements = caller_refinements, .return_eidx = return_eidx, .src = .{ .eidx = 8 } } }, results, ctx, &refinements);
        \\    try Inst.onFinish(results, ctx, &refinements);
        \\    Inst.backPropagate(results, &refinements, caller_refinements);
        \\    return return_eidx;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

