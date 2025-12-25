const std = @import("std");
const Context = @import("Context.zig");
const Inst = @import("Inst.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const EIdx = Inst.EIdx;

/// Debug function to dump analysis state at any point.
/// Call as: clr.dump(results, ctx, &refinements, caller_refinements, return_eidx);
pub fn dump(
    results: []Inst,
    ctx: *Context,
    refinements: *Refinements,
    caller_refinements: ?*Refinements,
    return_eidx: EIdx,
) void {
    const writer = ctx.writer;
    var buf: [4096]u8 = undefined;

    writer.writeAll("\n===== CLR DEBUG DUMP =====\n") catch {};

    // Context info
    writer.writeAll("Context:\n") catch {};
    const ctx_msg = std.fmt.bufPrint(&buf, "  file: {s}\n  function: {s}\n  line: {d}\n  column: {?d}\n", .{
        ctx.meta.file,
        ctx.meta.function,
        ctx.meta.line,
        ctx.meta.column,
    }) catch "  <format error>\n";
    writer.writeAll(ctx_msg) catch {};

    // Stacktrace
    writer.writeAll("  stacktrace: [") catch {};
    for (ctx.stacktrace.items, 0..) |frame, i| {
        if (i > 0) writer.writeAll(", ") catch {};
        writer.writeAll(frame) catch {};
    }
    writer.writeAll("]\n") catch {};

    // Return eidx
    const ret_msg = std.fmt.bufPrint(&buf, "Return EIdx: {d}\n", .{return_eidx}) catch "Return EIdx: <format error>\n";
    writer.writeAll(ret_msg) catch {};

    // Instruction results
    const inst_msg = std.fmt.bufPrint(&buf, "Instruction Results ({d} instructions):\n", .{results.len}) catch "Instructions: <format error>\n";
    writer.writeAll(inst_msg) catch {};
    dumpInstructionResults(writer, results, refinements, "  ");

    // Caller refinements
    if (caller_refinements) |cp| {
        writer.writeAll("Caller Return Slot:\n") catch {};
        var desc_buf: [2048]u8 = undefined;
        const desc = formatRefinementDeep(&desc_buf, cp.at(return_eidx).*, cp, 0);
        const line = std.fmt.bufPrint(&buf, "  [{d}] {s}\n", .{ return_eidx, desc }) catch "  <format error>\n";
        writer.writeAll(line) catch {};
    } else {
        writer.writeAll("Caller Refinements: null (entrypoint)\n") catch {};
    }

    writer.writeAll("===== END DEBUG DUMP =====\n\n") catch {};
}

fn dumpInstructionResults(writer: *std.Io.Writer, results: []Inst, refinements: *Refinements, prefix: []const u8) void {
    var desc_buf: [2048]u8 = undefined;
    var line_buf: [4096]u8 = undefined;

    for (results, 0..) |inst, i| {
        if (inst.refinement) |eidx| {
            const ref = refinements.at(eidx);
            const desc = formatRefinementDeep(&desc_buf, ref.*, refinements, 0);
            const line = std.fmt.bufPrint(&line_buf, "{s}[{d}] → {s}\n", .{ prefix, i, desc }) catch continue;
            writer.writeAll(line) catch {};
        } else {
            const line = std.fmt.bufPrint(&line_buf, "{s}[{d}] → (uninitialized)\n", .{ prefix, i }) catch continue;
            writer.writeAll(line) catch {};
        }
    }
}

fn formatRefinementDeep(buf: []u8, ref: Refinement, refinements: *Refinements, depth: usize) []const u8 {
    if (depth > 10) return "<max depth>";

    return switch (ref) {
        .scalar => |s| std.fmt.bufPrint(buf, "scalar(undef={s}, mem={s})", .{
            formatUndefined(s.undefined),
            formatMemSafety(s.memory_safety),
        }) catch "scalar(?)",
        .pointer => |p| blk: {
            var inner_buf: [1024]u8 = undefined;
            const pointee = refinements.at(p.to);
            const pointee_desc = formatRefinementDeep(&inner_buf, pointee.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "pointer(undef={s}, mem={s}) → {s}", .{
                formatUndefined(p.analyte.undefined),
                formatMemSafety(p.analyte.memory_safety),
                pointee_desc,
            }) catch "pointer(?)";
            break :blk result;
        },
        .optional => |o| blk: {
            var inner_buf: [1024]u8 = undefined;
            const payload = refinements.at(o.to);
            const payload_desc = formatRefinementDeep(&inner_buf, payload.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "optional(undef={s}, mem={s}) → {s}", .{
                formatUndefined(o.analyte.undefined),
                formatMemSafety(o.analyte.memory_safety),
                payload_desc,
            }) catch "optional(?)";
            break :blk result;
        },
        .region => |r| blk: {
            var inner_buf: [1024]u8 = undefined;
            const inner = refinements.at(r.to);
            const inner_desc = formatRefinementDeep(&inner_buf, inner.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "region → {s}", .{inner_desc}) catch "region(?)";
            break :blk result;
        },
        .@"struct" => "struct",
        .@"union" => "union",
        .retval_future => "retval_future",
        .unimplemented => "unimplemented",
        .void => "void",
        .noreturn => "noreturn",
    };
}

fn formatUndefined(undef: ?@import("analysis/undefined.zig").Undefined) []const u8 {
    if (undef) |u| {
        return switch (u) {
            .undefined => "UNDEF",
            .defined => "defined",
            .inconsistent => "CONFLICT",
        };
    }
    return "null";
}

fn formatMemSafety(ms: ?@import("analysis/memory_safety.zig").MemorySafety) []const u8 {
    if (ms) |m| {
        return switch (m) {
            .allocation => |a| if (a.freed != null) "freed" else "allocated",
            .stack_ptr => "stack_ptr",
            else => "OTHER",
        };
    }
    return "null";
}
