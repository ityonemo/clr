const std = @import("std");
const Context = @import("Context.zig");
const Inst = @import("Inst.zig");
const Refinements = @import("Refinements.zig");
const Refinement = Refinements.Refinement;
const Gid = Inst.Gid;
const State = @import("lib.zig").State;

/// Debug function to dump analysis state at any point.
/// Call as: clr.dump(state);
pub fn dump(state: State) void {
    const results = state.results;
    const ctx = state.ctx;
    const refinements = state.refinements;
    const return_gid = state.return_gid;

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

    // Return gid (with global refinements, return slot is in the same table)
    const ret_msg = std.fmt.bufPrint(&buf, "Return GID: {d}\n", .{return_gid}) catch "Return GID: <format error>\n";
    writer.writeAll(ret_msg) catch {};

    // Instruction results
    const inst_msg = std.fmt.bufPrint(&buf, "Instruction Results ({d} instructions):\n", .{results.len}) catch "Instructions: <format error>\n";
    writer.writeAll(inst_msg) catch {};
    dumpInstructionResults(writer, results, refinements, "  ");

    // Return slot info (from global refinements table)
    writer.writeAll("Return Slot:\n") catch {};
    var desc_buf: [2048]u8 = undefined;
    const desc = formatRefinementDeep(&desc_buf, return_gid, refinements.at(return_gid).*, refinements, 0);
    const line = std.fmt.bufPrint(&buf, "  {s}\n", .{desc}) catch "  <format error>\n";
    writer.writeAll(line) catch {};

    writer.writeAll("===== END DEBUG DUMP =====\n\n") catch {};
}

fn dumpInstructionResults(writer: *std.Io.Writer, results: []Inst, refinements: *Refinements, prefix: []const u8) void {
    var desc_buf: [2048]u8 = undefined;
    var line_buf: [4096]u8 = undefined;

    var uninit_start: ?usize = null;
    var i: usize = 0;
    while (i < results.len) : (i += 1) {
        const inst = results[i];
        if (inst.refinement) |gid| {
            // Flush any pending uninitialized range
            if (uninit_start) |start| {
                const end = i - 1;
                if (start == end) {
                    const line = std.fmt.bufPrint(&line_buf, "{s}[{d}] → (uninitialized)\n", .{ prefix, start }) catch continue;
                    writer.writeAll(line) catch {};
                } else {
                    const line = std.fmt.bufPrint(&line_buf, "{s}[{d}..{d}] → (uninitialized)\n", .{ prefix, start, end }) catch continue;
                    writer.writeAll(line) catch {};
                }
                uninit_start = null;
            }
            const ref = refinements.at(gid);
            const desc = formatRefinementDeep(&desc_buf, gid, ref.*, refinements, 0);
            const line = if (inst.name_id) |id|
                std.fmt.bufPrint(&line_buf, "{s}[{d}] name_id={d} → {s}\n", .{ prefix, i, id, desc }) catch continue
            else
                std.fmt.bufPrint(&line_buf, "{s}[{d}] → {s}\n", .{ prefix, i, desc }) catch continue;
            writer.writeAll(line) catch {};
        } else {
            // Start or continue uninitialized range
            if (uninit_start == null) {
                uninit_start = i;
            }
        }
    }
    // Flush any trailing uninitialized range
    if (uninit_start) |start| {
        const end = results.len - 1;
        if (start == end) {
            const line = std.fmt.bufPrint(&line_buf, "{s}[{d}] → (uninitialized)\n", .{ prefix, start }) catch return;
            writer.writeAll(line) catch {};
        } else {
            const line = std.fmt.bufPrint(&line_buf, "{s}[{d}..{d}] → (uninitialized)\n", .{ prefix, start, end }) catch return;
            writer.writeAll(line) catch {};
        }
    }
}

fn formatRefinementDeep(buf: []u8, gid: Gid, ref: Refinement, refinements: *Refinements, depth: usize) []const u8 {
    if (depth > 10) return "<max depth>";

    return switch (ref) {
        .scalar => |s| std.fmt.bufPrint(buf, "({d}) scalar(undef={s}, mem={s})", .{
            gid,
            formatUndefined(s.analyte.undefined),
            formatMemSafety(s.analyte.memory_safety),
        }) catch "scalar(?)",
        .pointer => |p| blk: {
            var inner_buf: [1024]u8 = undefined;
            const pointee = refinements.at(p.to);
            const pointee_desc = formatRefinementDeep(&inner_buf, p.to, pointee.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "({d}) pointer(undef={s}, mem={s}) → {s}", .{
                gid,
                formatUndefined(p.analyte.undefined),
                formatMemSafety(p.analyte.memory_safety),
                pointee_desc,
            }) catch "pointer(?)";
            break :blk result;
        },
        .optional => |o| blk: {
            var inner_buf: [1024]u8 = undefined;
            const payload = refinements.at(o.to);
            const payload_desc = formatRefinementDeep(&inner_buf, o.to, payload.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "({d}) optional(undef={s}, mem={s}) → {s}", .{
                gid,
                formatUndefined(o.analyte.undefined),
                formatMemSafety(o.analyte.memory_safety),
                payload_desc,
            }) catch "optional(?)";
            break :blk result;
        },
        .errorunion => |e| blk: {
            var inner_buf: [1024]u8 = undefined;
            const payload = refinements.at(e.to);
            const payload_desc = formatRefinementDeep(&inner_buf, e.to, payload.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "({d}) errorunion(undef={s}, mem={s}) → {s}", .{
                gid,
                formatUndefined(e.analyte.undefined),
                formatMemSafety(e.analyte.memory_safety),
                payload_desc,
            }) catch "errorunion(?)";
            break :blk result;
        },
        .region => |r| blk: {
            var inner_buf: [1024]u8 = undefined;
            const inner = refinements.at(r.to);
            const inner_desc = formatRefinementDeep(&inner_buf, r.to, inner.*, refinements, depth + 1);
            const result = std.fmt.bufPrint(buf, "({d}) region → {s}", .{ gid, inner_desc }) catch "region(?)";
            break :blk result;
        },
        .@"struct" => std.fmt.bufPrint(buf, "({d}) struct", .{gid}) catch "struct",
        .@"union" => std.fmt.bufPrint(buf, "({d}) union", .{gid}) catch "union",
        .unimplemented => std.fmt.bufPrint(buf, "({d}) unimplemented", .{gid}) catch "unimplemented",
        .void => std.fmt.bufPrint(buf, "({d}) void", .{gid}) catch "void",
        .noreturn => std.fmt.bufPrint(buf, "({d}) noreturn", .{gid}) catch "noreturn",
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
            .allocated => |a| if (a.freed != null) "freed" else "allocated",
            .stack => "stack",
            .unset => "unset",
        };
    }
    return "null";
}
