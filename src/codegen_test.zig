const std = @import("std");
const codegen = @import("codegen.zig");
const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
const InternPool = compiler.InternPool;
const Tag = Air.Inst.Tag;
const Data = Air.Inst.Data;

// Test AllocatorVTable that wraps testing.allocator for unit testing.
// This enables leak detection in tests.
const testing_allocator = std.testing.allocator;

fn testAlloc(_: ?[*]u8, len: usize, alignment: usize, ret_addr: usize) callconv(.c) ?[*]u8 {
    return @ptrCast(testing_allocator.rawAlloc(len, @enumFromInt(alignment), ret_addr));
}

fn testResize(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, new_len: usize, ret_addr: usize) callconv(.c) bool {
    return testing_allocator.rawResize(ptr.?[0..len], @enumFromInt(alignment), new_len, ret_addr);
}

fn testRemap(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, new_len: usize, ret_addr: usize) callconv(.c) ?[*]u8 {
    return testing_allocator.rawRemap(ptr.?[0..len], @enumFromInt(alignment), new_len, ret_addr);
}

fn testFree(_: ?[*]u8, ptr: ?[*]u8, len: usize, alignment: usize, ret_addr: usize) callconv(.c) void {
    return testing_allocator.rawFree(ptr.?[0..len], @enumFromInt(alignment), ret_addr);
}

const test_avt: clr_allocator.AllocatorVTable = .{
    .alloc = testAlloc,
    .resize = testResize,
    .remap = testRemap,
    .free = testFree,
};

fn initTestAllocator() void {
    clr_allocator.init(&test_avt);
}

// Dummy InternPool pointer for tests that don't need it
const dummy_ip: *const InternPool = @ptrFromInt(0x1000);

test "slotLine for dbg_stmt" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .dbg_stmt = .{ .line = 10, .column = 5 } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .dbg_stmt, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .dbg_stmt = .{ .line = 10, .column = 5 } }, tracked, 0, ctx, &payloads);\n", result);
}

test "slotLine for arg" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum, 0, &.{}, &.{}, &.{}, &.{"test_param"}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg0, .name = \"test_param\" } }, tracked, 0, ctx, &payloads);\n", result);
}

test "slotLine for arg with sequential zir_param_index uses arg counter" {
    // Normal case: zir_param_index values are sequential (0, 1, 2).
    // The arg_counter should track these correctly.
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const param_names = &[_][]const u8{ "a", "b", "c" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg0, .name = \"a\" } }, tracked, 0, ctx, &payloads);\n", result0);

    // Second arg: zir_param_index=1, should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 1 } };
    const result1 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg1, .name = \"b\" } }, tracked, 1, ctx, &payloads);\n", result1);

    // Third arg: zir_param_index=2, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result2 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg2, .name = \"c\" } }, tracked, 2, ctx, &payloads);\n", result2);

    try std.testing.expectEqual(@as(u32, 3), arg_counter);
}

test "slotLine for arg with non-sequential zir_param_index uses sequential arg counter" {
    // When comptime params are present, zir_param_index can have gaps (e.g., 0, 2, 3).
    // The arg_counter ensures we generate sequential arg references (arg0, arg1, arg2)
    // while still using zir_param_index for name lookup.
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Simulate 3 runtime args with zir_param_index 0, 2, 3 (gap at 1 = comptime param)
    const param_names = &[_][]const u8{ "self", "comptime_skip", "byte_count", "return_address" };
    var arg_counter: u32 = 0;

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum0, 0, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg0, .name = \"self\" } }, tracked, 0, ctx, &payloads);\n", result0);

    // Second arg: zir_param_index=2 (skipped 1), should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result1 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum1, 1, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg1, .name = \"byte_count\" } }, tracked, 1, ctx, &payloads);\n", result1);

    // Third arg: zir_param_index=3, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 3 } };
    const result2 = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum2, 2, &.{}, &.{}, &.{}, param_names, &arg_counter);
    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg2, .name = \"return_address\" } }, tracked, 2, ctx, &payloads);\n", result2);

    // Counter should be at 3 after processing 3 args
    try std.testing.expectEqual(@as(u32, 3), arg_counter);
}

test "slotLine for ret_safe with source" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .ret_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .ret_safe = .{ .caller_payloads = caller_payloads, .return_eidx = return_eidx, .src = 5 } }, tracked, 0, ctx, &payloads);\n", result);
}

test "slotLine for alloc" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .no_op = {} };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .alloc, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .alloc = .{} }, tracked, 0, ctx, &payloads);\n", result);
}

test "slotLine for store_safe" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const ptr_ref: Ref = @enumFromInt(@as(u32, 3) | (1 << 31));
    const val_ref: Ref = @enumFromInt(@as(u32, 4) | (1 << 31));
    const datum: Data = .{ .bin_op = .{ .lhs = ptr_ref, .rhs = val_ref } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .store_safe, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .store_safe = .{ .ptr = 3, .src = 4, .is_undef = false } }, tracked, 0, ctx, &payloads);\n", result);
}

test "slotLine for load" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .load, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .load = .{ .ptr = 5 } }, tracked, 0, ctx, &payloads);\n", result);
}

test "generateFunction produces complete function" {
    initTestAllocator();
    defer clr_allocator.deinit();

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
        \\fn fn_42(ctx: *Context, caller_payloads: ?*slots.Payloads) anyerror!slots.EIdx {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\
        \\    var payloads = slots.Payloads.init(ctx.allocator);
        \\    defer payloads.deinit();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 4);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    const return_eidx: slots.EIdx = if (caller_payloads) |cp| try cp.initEntity() else 0;
        \\
        \\    try Slot.apply(.{ .alloc = .{} }, tracked, 0, ctx, &payloads);
        \\    try Slot.apply(.{ .dbg_stmt = .{ .line = 1, .column = 3 } }, tracked, 1, ctx, &payloads);
        \\    try Slot.apply(.{ .load = .{ .ptr = 0 } }, tracked, 2, ctx, &payloads);
        \\    try Slot.apply(.{ .ret_safe = .{ .caller_payloads = caller_payloads, .return_eidx = return_eidx, .src = 2 } }, tracked, 3, ctx, &payloads);
        \\    try slots.onFinish(tracked, ctx, &payloads);
        \\    return return_eidx;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "epilogue generates correct output" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const result = codegen.epilogue(123);

    const expected =
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const slots = clr.slots;
        \\const Slot = slots.Slot;
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

test "slotLine for br with block and operand" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // br: block_inst = 3, operand = slot 8
    // Ref encoding: high bit set = instruction index, lower 31 bits = index
    const operand_ref: Air.Inst.Ref = @enumFromInt((1 << 31) | 8);
    const datum: Data = .{ .br = .{
        .block_inst = @enumFromInt(3),
        .operand = operand_ref,
    } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .br, datum, 9, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .br = .{ .block = 3, .src = 8 } }, tracked, 9, ctx, &payloads);\n", result);
}
