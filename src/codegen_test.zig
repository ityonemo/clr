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
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .dbg_stmt, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .dbg_stmt = .{ .line = 10, .column = 5 } }, tracked, 0, ctx);\n", result);
}

test "slotLine for arg" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .no_op = {} };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .arg, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .arg = .{ .value = arg0 } }, tracked, 0, ctx);\n", result);
}

test "slotLine for ret_safe with source" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .un_op = operand_ref };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .ret_safe, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .ret_safe = .{ .retval_ptr = &retval, .src = 5 } }, tracked, 0, ctx);\n", result);
}

test "slotLine for alloc" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ .no_op = {} };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .alloc, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .alloc = .{} }, tracked, 0, ctx);\n", result);
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
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .store_safe, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .store_safe = .{ .ptr = 3, .is_undef = false } }, tracked, 0, ctx);\n", result);
}

test "slotLine for load" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const Ref = Air.Inst.Ref;
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const datum: Data = .{ .ty_op = .{ .ty = .none, .operand = operand_ref } };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .load, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .load = .{ .ptr = 5 } }, tracked, 0, ctx);\n", result);
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
    const result = codegen.generateFunction(42, "test.main", dummy_ip, tags, data, &.{}, 10, "test.zig");

    const expected =
        \\fn fn_42(ctx: *Context) anyerror!Slot {
        \\    ctx.file = "test.zig";
        \\    ctx.base_line = 10;
        \\    try ctx.push("test.main");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 4);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .alloc = .{} }, tracked, 0, ctx);
        \\    try Slot.apply(.{ .dbg_stmt = .{ .line = 1, .column = 3 } }, tracked, 1, ctx);
        \\    try Slot.apply(.{ .load = .{ .ptr = 0 } }, tracked, 2, ctx);
        \\    try Slot.apply(.{ .ret_safe = .{ .retval_ptr = &retval, .src = 2 } }, tracked, 3, ctx);
        \\    retval = retval;
        \\    return retval;
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
        \\pub fn main() void {
        \\    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        \\    defer _ = gpa.deinit();
        \\    const allocator = gpa.allocator();
        \\    var ctx = Context.init(allocator);
        \\    defer ctx.deinit();
        \\    _ = fn_123(&ctx) catch std.process.exit(1);
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}
