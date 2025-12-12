const std = @import("std");
const codegen = @import("codegen.zig");
const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
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
    const result = codegen.generateFunction(42, "mymodule.myfunction", tags, data);

    const expected =
        \\fn fn_42(ctx: *Context) !void {
        \\    try ctx.push("mymodule.myfunction");
        \\    defer ctx.pop();
        \\
        \\    var slots = Slot.init(ctx.allocator, 1);
        \\    defer Slot.deinit(slots, ctx.allocator);
        \\
        \\    slots[0] = Slot.apply(.dbg_stmt, slots, ctx, .{ .line = 10, .column = 5 });
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
    const result = codegen.generateFunction(0, "root.main", tags, data);

    const expected =
        \\fn fn_0(ctx: *Context) !void {
        \\    try ctx.push("root.main");
        \\    defer ctx.pop();
        \\
        \\    var slots = Slot.init(ctx.allocator, 1);
        \\    defer Slot.deinit(slots, ctx.allocator);
        \\
        \\    slots[0] = Slot.apply(.alloc, slots, ctx, .{});
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

// Note: Empty instructions will panic - this is intentional as real functions always have instructions

test "epilogue generates correct output" {
    initTestAllocator();
    defer clr_allocator.deinit();

    const result = codegen.epilogue(123);
    try std.testing.expect(result != null);

    const expected =
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Slot = clr.Slot;
        \\
        \\pub fn main() !void {
        \\    var ctx = Context.init(std.heap.page_allocator);
        \\    defer ctx.deinit();
        \\    try fn_123(&ctx);
        \\}
        \\
    ;
    try std.testing.expectEqualStrings(expected, result.?);
}