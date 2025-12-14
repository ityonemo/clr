const std = @import("std");
const codegen = @import("codegen.zig");
const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
const InternPool = compiler.InternPool;
const Tag = Air.Inst.Tag;
const Data = Air.Inst.Data;

// Test AllocatorVTable that wraps page_allocator for unit testing.
// In tests, we don't have DLL relocation issues, so this works fine.
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

fn initTestAllocator() void {
    clr_allocator.init(&test_avt);
}

test "generateFunction generates correct output for dbg_stmt" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const tags: []const Tag = &.{.dbg_stmt};
    const data: []const Data = &.{.{ .dbg_stmt = .{ .line = 10, .column = 5 } }};
    // InternPool not used for dbg_stmt, so pass undefined pointer (aligned)
    const dummy_ip: *const InternPool = @ptrFromInt(0x1000);
    const extra: []const u32 = &.{};
    const result = codegen.generateFunction(42, "mymodule.myfunction", dummy_ip, tags, data, extra, 100, "test.zig");

    const expected =
        \\fn fn_42(ctx: *Context) anyerror!Slot {
        \\    ctx.file = "test.zig";
        \\    ctx.base_line = 100;
        \\    try ctx.push("mymodule.myfunction");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 1);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .dbg_stmt = .{ .line = 10, .column = 5 } }, tracked, 0, ctx);
        \\    retval = retval;
        \\    return retval;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "generateFunction generates correct output for arg" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const tags: []const Tag = &.{.arg};
    const data: []const Data = &.{.{ .no_op = {} }};
    const dummy_ip: *const InternPool = @ptrFromInt(0x1000);
    const extra: []const u32 = &.{};
    const result = codegen.generateFunction(0, "root.add_one", dummy_ip, tags, data, extra, 0, "root.zig");

    const expected =
        \\fn fn_0(ctx: *Context, arg0: *Slot) anyerror!Slot {
        \\    ctx.file = "root.zig";
        \\    ctx.base_line = 0;
        \\    try ctx.push("root.add_one");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 1);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .arg = .{ .value = arg0 } }, tracked, 0, ctx);
        \\    retval = retval;
        \\    return retval;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "generateFunction generates correct output for ret_safe" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const Ref = Air.Inst.Ref;
    const tags: []const Tag = &.{.ret_safe};
    // ret_safe uses un_op: operand is the return value slot (slot 5)
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const data: []const Data = &.{.{ .un_op = operand_ref }};
    const dummy_ip: *const InternPool = @ptrFromInt(0x1000);
    const extra: []const u32 = &.{};
    const result = codegen.generateFunction(0, "root.main", dummy_ip, tags, data, extra, 0, "root.zig");

    const expected =
        \\fn fn_0(ctx: *Context) anyerror!Slot {
        \\    ctx.file = "root.zig";
        \\    ctx.base_line = 0;
        \\    try ctx.push("root.main");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 1);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .ret_safe = .{ .retval_ptr = &retval, .src = 5 } }, tracked, 0, ctx);
        \\    retval = retval;
        \\    return retval;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "generateFunction generates correct output for alloc" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const tags: []const Tag = &.{.alloc};
    const data: []const Data = &.{.{ .no_op = {} }};
    // InternPool not used for alloc, so pass dummy pointer (aligned)
    const dummy_ip: *const InternPool = @ptrFromInt(0x1000);
    const extra: []const u32 = &.{};
    const result = codegen.generateFunction(0, "root.main", dummy_ip, tags, data, extra, 0, "root.zig");

    const expected =
        \\fn fn_0(ctx: *Context) anyerror!Slot {
        \\    ctx.file = "root.zig";
        \\    ctx.base_line = 0;
        \\    try ctx.push("root.main");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 1);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .alloc = .{} }, tracked, 0, ctx);
        \\    retval = retval;
        \\    return retval;
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

// Note: Empty instructions will panic - this is intentional as real functions always have instructions

test "generateFunction generates correct output for load" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const Ref = Air.Inst.Ref;
    const tags: []const Tag = &.{.load};
    // load uses ty_op: operand is the pointer (slot 5), ty is result type
    // Ref encoding: bit 31 set indicates instruction index, lower bits are the index
    const operand_ref: Ref = @enumFromInt(@as(u32, 5) | (1 << 31));
    const data: []const Data = &.{.{ .ty_op = .{ .ty = .none, .operand = operand_ref } }};
    const dummy_ip: *const InternPool = @ptrFromInt(0x1000);
    const extra: []const u32 = &.{};
    const result = codegen.generateFunction(0, "root.main", dummy_ip, tags, data, extra, 0, "root.zig");

    const expected =
        \\fn fn_0(ctx: *Context) anyerror!Slot {
        \\    ctx.file = "root.zig";
        \\    ctx.base_line = 0;
        \\    try ctx.push("root.main");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, 1);
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{};
        \\
        \\    try Slot.apply(.{ .load = .{ .ptr = 5 } }, tracked, 0, ctx);
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