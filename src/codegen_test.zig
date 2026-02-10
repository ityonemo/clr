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

const clr = @import("clr.zig");

// Empty field map for tests that don't need field mappings
var empty_field_map = clr.FieldHashMap{};

/// Create a test FnInfo with default/test values
fn testFnInfo(
    arena: std.mem.Allocator,
    name_map: *std.AutoHashMapUnmanaged(u32, []const u8),
    field_map: *clr.FieldHashMap,
    tags: []const Tag,
    data: []const Data,
    extra: []const u32,
    param_names: []const []const u8,
) codegen.FnInfo {
    return .{
        .arena = arena,
        .name_map = name_map,
        .field_map = field_map,
        .ip = dummy_ip,
        .tags = tags,
        .data = data,
        .extra = extra,
        .param_names = param_names,
    };
}

/// Check if name_map contains a specific name (by value, not key)
fn nameMapContains(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), name: []const u8) bool {
    var it = name_map.iterator();
    while (it.next()) |entry| {
        if (std.mem.eql(u8, entry.value_ptr.*, name)) {
            return true;
        }
    }
    return false;
}

test "instLine for dbg_stmt" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .dbg_stmt = .{ .line = 10, .column = 5 } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .dbg_stmt, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .dbg_stmt = .{ .line = 10, .column = 5 } });\n", result);
}

test "instLine for arg" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{"test_param"});
    const result = codegen._instLine(&info, .arg, datum, 0, null);

    // Name "test_param" gets registered with a hash-based ID
    try std.testing.expect(std.mem.startsWith(u8, result, "    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name_id = "));
    try std.testing.expect(nameMapContains(&name_map, "test_param"));
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

    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, param_names);

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(&info, .arg, datum0, 0, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result0, "    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name_id = "));

    // Second arg: zir_param_index=1, should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 1 } };
    const result1 = codegen._instLine(&info, .arg, datum1, 1, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result1, "    try Inst.apply(state, 1, .{ .arg = .{ .value = arg1, .name_id = "));

    // Third arg: zir_param_index=2, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result2 = codegen._instLine(&info, .arg, datum2, 2, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result2, "    try Inst.apply(state, 2, .{ .arg = .{ .value = arg2, .name_id = "));

    try std.testing.expectEqual(@as(u32, 3), arg_counter);

    // Verify all names were registered in the map
    try std.testing.expect(nameMapContains(&name_map, "a"));
    try std.testing.expect(nameMapContains(&name_map, "b"));
    try std.testing.expect(nameMapContains(&name_map, "c"));
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

    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, param_names);

    // First arg: zir_param_index=0, should become arg0
    const datum0: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 0 } };
    const result0 = codegen._instLine(&info, .arg, datum0, 0, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result0, "    try Inst.apply(state, 0, .{ .arg = .{ .value = arg0, .name_id = "));

    // Second arg: zir_param_index=2 (skipped 1), should become arg1
    const datum1: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 2 } };
    const result1 = codegen._instLine(&info, .arg, datum1, 1, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result1, "    try Inst.apply(state, 1, .{ .arg = .{ .value = arg1, .name_id = "));

    // Third arg: zir_param_index=3, should become arg2
    const datum2: Data = .{ .arg = .{ .ty = .none, .zir_param_index = 3 } };
    const result2 = codegen._instLine(&info, .arg, datum2, 2, &arg_counter);
    try std.testing.expect(std.mem.startsWith(u8, result2, "    try Inst.apply(state, 2, .{ .arg = .{ .value = arg2, .name_id = "));

    // Counter should be at 3 after processing 3 args
    try std.testing.expectEqual(@as(u32, 3), arg_counter);

    // Verify correct names were registered (self, byte_count, return_address - not comptime_skip)
    try std.testing.expect(nameMapContains(&name_map, "self"));
    try std.testing.expect(nameMapContains(&name_map, "byte_count"));
    try std.testing.expect(nameMapContains(&name_map, "return_address"));
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .ret_safe, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .ret_safe = .{ .src = .{ .inst = 5 } } });\n", result);
}

test "instLine for alloc" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // alloc uses datum.ty which is a pointer type - use well-known manyptr_u8_type
    const datum: Data = .{ .ty = .{ .ip_index = .manyptr_u8_type } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .alloc, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .store_safe, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 3 }, .src = .{ .inst = 4 } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .load, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .load = .{ .ptr = .{ .inst = 5 } } });\n", result);
}

test "generateFunction produces complete function" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, tags, data, extra, &.{});
    const result = codegen.generateFunction(42, "test.main", &info, 10, "test.zig");

    const expected =
        \\fn fn_42(ctx: *Context, refinements: *Refinements, return_gid: Gid) anyerror!Gid {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\    refinements.testValid();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 4);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\
        \\    var early_returns = @import("std").ArrayListUnmanaged(State){};
        \\    defer Inst.freeEarlyReturns(&early_returns, ctx.allocator);
        \\
        \\    const base_gid: Gid = @intCast(refinements.list.items.len);
        \\    const state = State{ .ctx = ctx, .results = results, .refinements = refinements, .return_gid = return_gid, .base_gid = base_gid, .early_returns = &early_returns };
        \\
        \\    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 1, .{ .dbg_stmt = .{ .line = 1, .column = 3 } });
        \\    try Inst.apply(state, 2, .{ .load = .{ .ptr = .{ .inst = 0 } } });
        \\    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .inst = 2 } } });
        \\    try Inst.mergeEarlyReturns(state);
        \\    try Inst.onFinish(state);
        \\    return return_gid;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "epilogue generates correct output with typed return slot" {
    initTestAllocator();
    defer deinitTestAllocator();

    const result = codegen.epilogue(123, ".{ .ty = .{ .scalar = {} } }");

    const expected =
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Inst = clr.Inst;
        \\const Refinements = clr.Refinements;
        \\const Gid = clr.Gid;
        \\const Src = clr.Src;
        \\const State = clr.State;
        \\
        \\const global_defs = [_]clr.GlobalDef{
        \\};
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
        \\    ctx.getName = &getName;
        \\    ctx.getFieldId = &getFieldId;
        \\
        \\    var refinements = Refinements.init(allocator);
        \\    defer refinements.deinit();
        \\    const return_type: clr.Type = .{ .ty = .{ .scalar = {} } };
        \\    const return_ref = clr.typeToRefinement(return_type, &refinements) catch Refinements.Refinement{ .scalar = .{} };
        \\    const return_gid = refinements.appendEntity(return_ref) catch 0;
        \\    clr.splatInit(&refinements, return_gid, &ctx);
        \\
        \\    _ = fn_123(&ctx, &refinements, return_gid) catch {
        \\        file_writer.interface.flush() catch {};
        \\        std.process.exit(1);
        \\    };
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

// =============================================================================
// Name/Field Lookup Tests
// =============================================================================

test "emitGetName generates switch for name mappings" {
    initTestAllocator();
    defer deinitTestAllocator();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};
    name_map.put(clr_allocator.allocator(), 1, "foo") catch unreachable;

    const result = codegen.emitGetName(&name_map);

    const expected =
        \\pub fn getName(id: u32) []const u8 {
        \\    return switch (id) {
        \\        1 => "foo",
        \\        else => "unknown",
        \\    };
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "emitGetName generates default function for empty map" {
    initTestAllocator();
    defer deinitTestAllocator();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};
    const result = codegen.emitGetName(&name_map);

    const expected =
        \\pub fn getName(id: u32) []const u8 {
        \\    return switch (id) {
        \\        else => "unknown",
        \\    };
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "emitGetFieldId generates array lookup for field mappings" {
    initTestAllocator();
    defer deinitTestAllocator();

    var field_map = clr.FieldHashMap{};
    // Type 100 has fields 0 -> name 5, 1 -> name 6
    field_map.put(clr_allocator.allocator(), .{ .type_id = 100, .field_index = 0 }, 5) catch unreachable;
    field_map.put(clr_allocator.allocator(), .{ .type_id = 100, .field_index = 1 }, 6) catch unreachable;

    const result = codegen.emitGetFieldId(&field_map);

    const expected =
        \\pub fn getFieldId(type_id: u32, field_index: u32) ?u32 {
        \\    const fields: []const u32 = switch (type_id) {
        \\        100 => &.{ 5, 6 },
        \\        else => return null,
        \\    };
        \\    return fields[field_index];
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "emitGetFieldId generates default function for empty map" {
    initTestAllocator();
    defer deinitTestAllocator();

    var field_map = clr.FieldHashMap{};
    const result = codegen.emitGetFieldId(&field_map);

    const expected =
        \\pub fn getFieldId(type_id: u32, field_index: u32) ?u32 {
        \\    _ = type_id;
        \\    _ = field_index;
        \\    return null;
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .br, datum, 9, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 9, .{ .br = .{ .block = 3, .src = .{ .inst = 8 } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .bitcast, datum, 0, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 0, .{ .bitcast = .{ .src = .{ .inst = 7 }, .ty = .{ .ty = .{ .scalar = {} } } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .unwrap_errunion_payload, datum, 5, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 4 } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .optional_payload, datum, 6, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .optional_payload = .{ .src = .{ .inst = 3 } } });\n", result);
}

// =============================================================================
// DbgVar Tests (dbg_var_ptr, dbg_var_val, dbg_arg_inline)
// =============================================================================

fn makeExtraWithString(comptime s: []const u8) []const u32 {
    // Pack string into u32 array with null terminator
    const len_with_null = s.len + 1;
    const word_count = (len_with_null + 3) / 4;
    // Use comptime to create a static array
    const result = comptime blk: {
        var arr: [word_count]u32 = [_]u32{0} ** word_count;
        const bytes: [*]u8 = @ptrCast(&arr);
        @memcpy(bytes[0..s.len], s);
        // bytes[s.len] is already 0 from zero-init
        break :blk arr;
    };
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, extra, &.{});
    const result = codegen._instLine(&info, .dbg_var_ptr, datum, 3, null);

    // Check prefix is correct and name was registered
    try std.testing.expect(std.mem.startsWith(u8, result, "    try Inst.apply(state, 3, .{ .dbg_var_ptr = .{ .ptr = 2, .name_id = "));
    try std.testing.expect(nameMapContains(&name_map, "my_var"));
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, extra, &.{});
    const result = codegen._instLine(&info, .dbg_var_val, datum, 6, null);

    // Check prefix is correct and name was registered
    try std.testing.expect(std.mem.startsWith(u8, result, "    try Inst.apply(state, 6, .{ .dbg_var_val = .{ .ptr = 5, .name_id = "));
    try std.testing.expect(nameMapContains(&name_map, "value_var"));
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, extra, &.{});
    const result = codegen._instLine(&info, .dbg_arg_inline, datum, 2, null);

    // Check prefix is correct and name was registered
    try std.testing.expect(std.mem.startsWith(u8, result, "    try Inst.apply(state, 2, .{ .dbg_arg_inline = .{ .ptr = 1, .name_id = "));
    try std.testing.expect(nameMapContains(&name_map, "arg_name"));
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .unreach, datum, 7, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 7, .{ .unreach = .{} });\n", result);
}

test "instLine for bit_and (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .bit_and, datum, 3, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .bit_and = .{} });\n", result);
}

test "instLine for cmp_eq (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .cmp_eq, datum, 4, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_eq = .{} });\n", result);
}

test "instLine for cmp_gt (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .cmp_gt, datum, 4, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_gt = .{} });\n", result);
}

test "instLine for cmp_lte (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .cmp_lte, datum, 4, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .cmp_lte = .{} });\n", result);
}

test "instLine for ctz (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .ctz, datum, 2, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .ctz = .{} });\n", result);
}

test "instLine for sub (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .bin_op = .{ .lhs = .none, .rhs = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .sub, datum, 5, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .sub = .{} });\n", result);
}

test "instLine for is_non_err" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .is_non_err, datum, 6, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 6, .{ .is_non_err = .{ .src = .{ .inst = 5 } } });\n", result);
}

test "instLine for unwrap_errunion_err (Simple)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = .none } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .unwrap_errunion_err, datum, 8, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 8, .{ .unwrap_errunion_err = .{} });\n", result);
}

test "instLine for add_with_overflow (OverflowOp)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_pl = .{ .ty = .none, .payload = 0 } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .add_with_overflow, datum, 9, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 9, .{ .add_with_overflow = .{} });\n", result);
}

test "instLine for sub_with_overflow (OverflowOp)" {
    initTestAllocator();
    defer deinitTestAllocator();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    const datum: Data = .{ .ty_pl = .{ .ty = .none, .payload = 0 } };
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .sub_with_overflow, datum, 10, null);

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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .is_non_null, datum, 5, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 5, .{ .is_non_null = .{ .src = .{ .inst = 4 } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .is_null, datum, 4, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .is_null = .{ .src = .{ .inst = 3 } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .ret_load, datum, 5, null);

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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .struct_field_ptr_index_0, datum, 3, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .struct_field_ptr = .{ .base = .{ .inst = 2 }, .field_index = 0, .ty = .{ .ty = .{ .scalar = {} } } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .struct_field_ptr_index_1, datum, 4, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 4, .{ .struct_field_ptr = .{ .base = .{ .inst = 1 }, .field_index = 1, .ty = .{ .ty = .{ .scalar = {} } } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .get_union_tag, datum, 6, null);

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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .block, datum, 2, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .ty = .{ .void = {} } } } });\n", result);
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
    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, &.{}, &.{}, &.{}, &.{});
    const result = codegen._instLine(&info, .store, datum, 3, null);

    try std.testing.expectEqualStrings("    try Inst.apply(state, 3, .{ .store = .{ .ptr = .{ .inst = 1 }, .src = .{ .inst = 2 } } });\n", result);
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

    var arena = clr_allocator.newArena();
    defer arena.deinit();

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

    const info = testFnInfo(arena.allocator(), &name_map, &empty_field_map, tags, data, extra, &.{});
    const result = codegen.generateFunction(42, "test.main", &info, 10, "test.zig");

    // Expected output:
    // 1. Branch functions should be generated for true/false branches (named by cond_br index)
    // 2. Main function includes block instruction and condition load
    // 3. cond_br branches (instructions 4,5,6) are in sub functions only

    const expected =
        \\fn fn_42_cond_br_false_7(state: State) anyerror!void {
        \\    try Inst.apply(state, 0, .{ .cond_br = .{ .branch = false, .condition_idx = 3 } });
        \\    try Inst.apply(state, 6, .{ .br = .{ .block = 2, .src = .{ .int_const = .{ .ty = .{ .void = {} } } } } });
        \\}
        \\
        \\fn fn_42_cond_br_true_7(state: State) anyerror!void {
        \\    try Inst.apply(state, 0, .{ .cond_br = .{ .branch = true, .condition_idx = 3 } });
        \\    try Inst.apply(state, 4, .{ .store_safe = .{ .ptr = .{ .inst = 0 }, .src = .{ .inst = 3 } } });
        \\    try Inst.apply(state, 5, .{ .br = .{ .block = 2, .src = .{ .int_const = .{ .ty = .{ .void = {} } } } } });
        \\}
        \\
        \\fn fn_42(ctx: *Context, refinements: *Refinements, return_gid: Gid) anyerror!Gid {
        \\    ctx.meta.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push_fn("test.main");
        \\    defer ctx.pop_fn();
        \\    refinements.testValid();
        \\
        \\    const results = try Inst.make_results_list(ctx.allocator, 10);
        \\    defer Inst.clear_results_list(results, ctx.allocator);
        \\
        \\    var early_returns = @import("std").ArrayListUnmanaged(State){};
        \\    defer Inst.freeEarlyReturns(&early_returns, ctx.allocator);
        \\
        \\    const base_gid: Gid = @intCast(refinements.list.items.len);
        \\    const state = State{ .ctx = ctx, .results = results, .refinements = refinements, .return_gid = return_gid, .base_gid = base_gid, .early_returns = &early_returns };
        \\
        \\    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });
        \\    try Inst.apply(state, 1, .{ .store_safe = .{ .ptr = .{ .inst = 0 }, .src = .{ .int_const = .{ .ty = .{ .undefined = &.{ .ty = .{ .scalar = {} } } } } } } });
        \\    try Inst.apply(state, 2, .{ .block = .{ .ty = .{ .ty = .{ .void = {} } } } });
        \\    try Inst.apply(state, 3, .{ .load = .{ .ptr = .{ .int_const = .{ .ty = .{ .scalar = {} } } } } });
        \\    try Inst.apply(state, 4, .{ .noop = .{} });
        \\    try Inst.apply(state, 5, .{ .noop = .{} });
        \\    try Inst.apply(state, 6, .{ .noop = .{} });
        \\    try Inst.cond_br(state, 7, fn_42_cond_br_true_7, fn_42_cond_br_false_7);
        \\    try Inst.apply(state, 8, .{ .load = .{ .ptr = .{ .inst = 0 } } });
        \\    try Inst.apply(state, 9, .{ .ret_safe = .{ .src = .{ .inst = 8 } } });
        \\    try Inst.mergeEarlyReturns(state);
        \\    try Inst.onFinish(state);
        \\    return return_gid;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

