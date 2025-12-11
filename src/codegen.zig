const clr_allocator = @import("allocator.zig");
const compiler = @import("compiler");
const Air = compiler.Air;
const Tag = Air.Inst.Tag;

// =============================================================================
// Code Generation Templates
// =============================================================================
//
// Functions that generate Zig source code from AIR. Each function returns
// a heap-allocated string (via clr_allocator.allocPrint) or null on failure.
// =============================================================================

const std = @import("std");

/// Build all slot apply lines into a single buffer (uses provided arena allocator)
fn buildSlotLines(arena: std.mem.Allocator, instructions: []const Tag) []const u8 {
    if (instructions.len == 0) return "";

    // Build lines into a list first
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;

    for (instructions, 0..) |instr, i| {
        const opts = switch (instr) {
            .dbg_stmt => ".{ .context = ctx }",
            else => ".{}",
        };
        const line = clr_allocator.allocPrint(arena, "    slots[{d}] = Slot.apply(.{s}, slots, {s});\n", .{ i, @tagName(instr), opts }, null) orelse @panic("out of memory");
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

/// Generate a function stub
pub fn functionStub(func_index: u32, fqn: []const u8, instructions: []const Tag) ?[]u8 {
    if (instructions.len == 0) @panic("function with no instructions encountered");

    // Per-function arena for temporary allocations
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Build slot lines first
    const slot_lines = buildSlotLines(arena.allocator(), instructions);

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
    , .{ func_index, fqn, instructions.len, slot_lines }, size_hint);
}

/// Generate epilogue with imports and main function
pub fn epilogue(entrypoint_index: u32) ?[]u8 {
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\const std = @import("std");
        \\const Context = @import("context");
        \\const Slot = @import("slot").Slot;
        \\
        \\pub fn main() !void {{
        \\    var ctx = Context.init(std.heap.page_allocator);
        \\    defer ctx.deinit();
        \\    try fn_{d}(&ctx);
        \\}}
        \\
    , .{entrypoint_index}, null);
}

test {
    @import("std").testing.refAllDecls(@import("codegen_test.zig"));
}
