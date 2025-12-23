const std = @import("std");
const Context = @import("Context.zig");
const Refinements = @import("Refinements.zig");
const EIdx = @import("Inst.zig").EIdx;

/// Debug function to dump analysis state at any point.
/// Call as: clr.dump(ctx, &refinements, caller_refinements, return_eidx);
pub fn dump(
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

    // Local refinements
    const ref_msg = std.fmt.bufPrint(&buf, "Refinements ({d} entities):\n", .{refinements.list.items.len}) catch "Refinements: <format error>\n";
    writer.writeAll(ref_msg) catch {};
    dumpRefinementsList(writer, &buf, refinements, "  ");

    // Caller refinements
    if (caller_refinements) |cp| {
        const cp_msg = std.fmt.bufPrint(&buf, "Caller Refinements ({d} entities):\n", .{cp.list.items.len}) catch "Caller Refinements: <format error>\n";
        writer.writeAll(cp_msg) catch {};
        dumpRefinementsList(writer, &buf, cp, "  ");
    } else {
        writer.writeAll("Caller Refinements: null (entrypoint)\n") catch {};
    }

    writer.writeAll("===== END DEBUG DUMP =====\n\n") catch {};
}

fn dumpRefinementsList(writer: *std.Io.Writer, buf: []u8, refinements: *Refinements, prefix: []const u8) void {
    for (refinements.list.items, 0..) |ref, i| {
        const ref_str = formatRefinement(buf, ref);
        const line = std.fmt.bufPrint(buf[ref_str.len..], "{s}[{d}] {s}\n", .{ prefix, i, ref_str }) catch continue;
        writer.writeAll(line) catch {};
    }
}

fn formatRefinement(buf: []u8, ref: Refinements.Refinement) []const u8 {
    return switch (ref) {
        .scalar => |s| std.fmt.bufPrint(buf, "scalar(undef={s}, mem={s})", .{
            formatUndefined(s.undefined),
            formatMemSafety(s.memory_safety),
        }) catch "scalar(?)",
        .pointer => |p| std.fmt.bufPrint(buf, "pointer(to={d}, undef={s}, mem={s})", .{
            p.to,
            formatUndefined(p.analyte.undefined),
            formatMemSafety(p.analyte.memory_safety),
        }) catch "pointer(?)",
        .optional => |o| std.fmt.bufPrint(buf, "optional(to={d})", .{o.to}) catch "optional(?)",
        .region => |r| std.fmt.bufPrint(buf, "region(to={d})", .{r.to}) catch "region(?)",
        .@"struct" => "struct",
        .@"union" => "union",
        .retval_future => "retval_future",
        .future => "future",
        .unimplemented => "unimplemented",
        .void => "void",
    };
}

fn formatUndefined(undef: ?@import("analysis/undefined.zig").Undefined) []const u8 {
    if (undef) |u| {
        return switch (u) {
            .undefined => "UNDEF",
            .defined => "defined",
            .conflicting_branch => "CONFLICT",
        };
    }
    return "null";
}

fn formatMemSafety(ms: ?@import("analysis/memory_safety.zig").MemorySafety) []const u8 {
    if (ms) |m| {
        return switch (m) {
            .allocation => |a| if (a.freed != null) "freed" else "allocated",
            .stack_ptr => "stack_ptr",
        };
    }
    return "null";
}
