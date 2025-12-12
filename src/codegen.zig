const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
const InternPool = compiler.InternPool;
const Tag = Air.Inst.Tag;
const Data = Air.Inst.Data;
const Ref = Air.Inst.Ref;

// =============================================================================
// Code Generation Templates
// =============================================================================
//
// Functions that generate Zig source code from AIR. Each function returns
// a heap-allocated string (via clr_allocator.allocPrint), panicking on failure.
// =============================================================================

const std = @import("std");

/// Returns a keyword-safe tag name using @"..." syntax to handle reserved keywords
fn safeName(tag: Tag) []const u8 {
    return switch (tag) {
        .@"try" => "@\"try\"",
        else => @tagName(tag),
    };
}

/// Returns the payload string for a given tag and data
fn payload(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data) []const u8 {
    return switch (tag) {
        .dbg_stmt => payloadDbgStmt(arena, datum),
        .store_safe => payloadStoreSafe(arena, ip, datum),
        .load => payloadLoad(arena, datum),
        else => ".{}",
    };
}

fn payloadDbgStmt(arena: std.mem.Allocator, datum: Data) []const u8 {
    return clr_allocator.allocPrint(arena, ".{{ .line = {d}, .column = {d} }}", .{
        datum.dbg_stmt.line,
        datum.dbg_stmt.column,
    }, null);
}

fn payloadStoreSafe(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    const lhs_index = datum.bin_op.lhs.toIndex() orelse return ".{}";
    const ptr = @intFromEnum(lhs_index);
    const is_undef = isUndefRef(ip, datum.bin_op.rhs);
    return clr_allocator.allocPrint(arena, ".{{ .ptr = {d}, .is_undef = {} }}", .{ ptr, is_undef }, null);
}

fn payloadLoad(arena: std.mem.Allocator, datum: Data) []const u8 {
    const operand_index = datum.ty_op.operand.toIndex() orelse return ".{}";
    const ptr = @intFromEnum(operand_index);
    return clr_allocator.allocPrint(arena, ".{{ .ptr = {d} }}", .{ptr}, null);
}

/// Detect if a Ref is an undefined value (typed or untyped).
/// Uses InternPool to detect typed undef like `undefined` of type `u8`.
fn isUndefRef(ip: *const InternPool, ref: Ref) bool {
    // First check for untyped undef constants
    switch (ref) {
        .undef, .undef_bool, .undef_usize, .undef_u1 => return true,
        else => {},
    }
    // Check for typed undef via InternPool lookup
    const index = ref.toInterned() orelse return false;
    return ip.isUndef(index);
}

/// Build all slot apply lines into a single buffer (uses provided arena allocator)
fn buildSlotLines(arena: std.mem.Allocator, ip: *const InternPool, tags: []const Tag, data: []const Data) []const u8 {
    if (tags.len == 0) return "";

    // Build lines into a list first
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;

    for (tags, data, 0..) |tag, datum, i| {
        const line = clr_allocator.allocPrint(arena, "    slots[{d}] = try Slot.apply(.{s}, slots, ctx, {s});\n", .{ i, safeName(tag), payload(arena, ip, tag, datum) }, null);
        lines.append(arena, line) catch @panic("out of memory");
        total_len += line.len;
    }

    // Concatenate all lines into single buffer
    const buf = arena.alloc(u8, total_len) catch @panic("out of memory");
    var pos: usize = 0;
    for (lines.items) |line| {
        @memcpy(buf[pos..][0..line.len], line);
        pos += line.len;
    }

    return buf;
}

/// Generate Zig source code for a function from AIR instructions
pub fn generateFunction(func_index: u32, fqn: []const u8, ip: *const InternPool, tags: []const Tag, data: []const Data) []u8 {
    if (tags.len == 0) @panic("function with no instructions encountered");

    // Per-function arena for temporary allocations
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Build slot lines first
    const slot_lines = buildSlotLines(arena.allocator(), ip, tags, data);

    // Generate complete function with slot lines injected (use main arena for final result)
    // Size hint: slot_lines + ~200 bytes for template + some margin
    const size_hint = slot_lines.len + 512;
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\fn fn_{d}(ctx: *Context) !void {{
        \\    try ctx.push("{s}");
        \\    defer ctx.pop();
        \\
        \\    var slots = Slot.init(ctx.allocator, {d});
        \\    defer Slot.deinit(slots, ctx.allocator);
        \\
        \\{s}}}
        \\
    , .{ func_index, fqn, tags.len, slot_lines }, size_hint);
}

/// Generate epilogue with imports and main function
pub fn epilogue(entrypoint_index: u32) []u8 {
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Slot = clr.Slot;
        \\
        \\pub fn main() void {{
        \\    var ctx = Context.init(std.heap.page_allocator);
        \\    defer ctx.deinit();
        \\    fn_{d}(&ctx) catch std.process.exit(1);
        \\}}
        \\
    , .{entrypoint_index}, null);
}

test {
    @import("std").testing.refAllDecls(@import("codegen_test.zig"));
}
