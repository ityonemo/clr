const std = @import("std");
const tag = @import("tag.zig");
const Refinements = @import("Refinements.zig");
const Inst = @import("Inst.zig");
const core = @import("core.zig");
const TraceType = @import("Trace.zig").Trace;

allocator: std.mem.Allocator,
stacktrace: Trace,
trace_arena: std.heap.ArenaAllocator,
meta: core.Meta,
base_line: u32 = 0,
writer: *std.Io.Writer = undefined,

/// Global ID counter for refinement provenance tracking.
/// Incremented each time a new refinement entity is created.
next_gid: Refinements.Gid = 0,

/// Name lookup function pointer - set by generated .air.zig main()
/// Converts a Name ID (u32) to a string slice
getName: *const fn (u32) []const u8 = undefined,

/// Field name lookup function pointer - set by generated .air.zig main()
/// Converts (type_id, field_index) to a name_id for getName
/// Returns null for tuple types or unknown fields
getFieldId: *const fn (u32, u32) ?u32 = undefined,

/// Arena for error path names - strings allocated here are freed all at once at end of run
error_name_arena: std.heap.ArenaAllocator,

/// Stack of active loop frames for tracking nested loop exits
loop_stack: std.ArrayListUnmanaged(LoopFrame),

/// Frame for tracking loop state during fixed-point iteration
pub const LoopFrame = struct {
    block_idx: usize, // which block this loop owns (for br matching)
    br_states: std.AutoHashMapUnmanaged(usize, std.ArrayListUnmanaged(BrState)), // target_block -> exit states

    /// Track regions being iterated in THIS loop (freed without pre-freed state)
    /// Used to suppress double-free false positives for region cleanup loops
    deferred_region_gids: std.AutoHashMapUnmanaged(Refinements.Gid, void),

    /// Snapshot of refinements before loop for null-case comparison
    /// Used to detect if a region was already freed BEFORE the loop
    pre_loop_refinements: ?*Refinements,

    pub const BrState = struct {
        results: []Inst,
        refinements: *Refinements,
    };

    pub fn init(allocator: std.mem.Allocator, pre_loop_refs: *Refinements) error{OutOfMemory}!LoopFrame {
        // Clone the refinements to capture pre-loop state
        const pre_loop_snapshot = try allocator.create(Refinements);
        pre_loop_snapshot.* = try pre_loop_refs.clone(allocator);
        return .{
            .block_idx = 0,
            .br_states = .{},
            .deferred_region_gids = .{},
            .pre_loop_refinements = pre_loop_snapshot,
        };
    }

    pub fn deinit(self: *LoopFrame, allocator: std.mem.Allocator) void {
        var it = self.br_states.iterator();
        while (it.next()) |entry| {
            for (entry.value_ptr.items) |br_state| {
                allocator.free(br_state.results);
                br_state.refinements.deinit();
                allocator.destroy(br_state.refinements);
            }
            entry.value_ptr.deinit(allocator);
        }
        self.br_states.deinit(allocator);
        self.deferred_region_gids.deinit(allocator);
        if (self.pre_loop_refinements) |pre_loop| {
            pre_loop.deinit();
            allocator.destroy(pre_loop);
        }
    }
};

const Context = @This();

pub const Trace = TraceType;

/// Default getName for tests - returns "unknown"
fn testGetName(_: u32) []const u8 {
    return "unknown";
}

/// Default getFieldId for tests - returns null
fn testGetFieldId(_: u32, _: u32) ?u32 {
    return null;
}

pub fn init(allocator: std.mem.Allocator, writer: *std.Io.Writer) Context {
    return .{
        .allocator = allocator,
        .stacktrace = .{},
        .trace_arena = std.heap.ArenaAllocator.init(allocator),
        .meta = .{
            .function = "",
            .file = "",
            .line = 0,
            .column = null,
        },
        .writer = writer,
        .getName = &testGetName,
        .getFieldId = &testGetFieldId,
        .error_name_arena = std.heap.ArenaAllocator.init(allocator),
        .loop_stack = .{},
    };
}

pub fn deinit(self: *Context) void {
    self.trace_arena.deinit();
    self.error_name_arena.deinit();
    for (self.loop_stack.items) |*frame| {
        frame.deinit(self.allocator);
    }
    self.loop_stack.deinit(self.allocator);
}

/// Build a full access path name for an instruction by walking the tag chain.
/// Allocated from error_name_arena. Returns null if no name can be determined.
pub fn buildPathName(self: *Context, results: []const Inst, refinements: *Refinements, index: usize) ?[]const u8 {
    const inst = results[index];

    // Check for root variable name (set by dbg_var_ptr or load lookahead)
    if (inst.name_id) |name_id| {
        return self.getName(name_id);
    }

    // Walk the tag chain to build compound paths
    const t = inst.inst_tag orelse return null;
    switch (t) {
        .struct_field_ptr => |sfp| {
            const base = switch (sfp.base) {
                .inst => |inst_idx| inst_idx,
                .interned, .fnptr => return null, // global/constant base - no path name
            };
            const base_path = self.buildPathName(results, refinements, base);

            // Get field name from container's type_id
            const base_ref_idx = results[base].refinement orelse return base_path;
            const type_id = switch (refinements.at(base_ref_idx).*) {
                .pointer => |p| blk: {
                    // struct_field_ptr's base is a pointer to struct/union
                    const pointee = refinements.at(p.info.to);
                    break :blk switch (pointee.*) {
                        .@"struct" => |s| s.type_id,
                        .@"union" => |u| u.type_id,
                        else => return base_path,
                    };
                },
                else => return base_path,
            };
            const field_name_id = self.getFieldId(type_id, @intCast(sfp.field_index)) orelse return base_path;
            const field_name = self.getName(field_name_id);

            if (base_path) |bp| {
                const arena_alloc = self.error_name_arena.allocator();
                return std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ bp, field_name }) catch return field_name;
            }
            return field_name;
        },
        .struct_field_val => |sfv| {
            const operand = sfv.operand orelse return null;
            const base_path = self.buildPathName(results, refinements, operand);

            // Get field name from container's type_id
            const base_ref_idx = results[operand].refinement orelse return base_path;
            const type_id = switch (refinements.at(base_ref_idx).*) {
                .@"struct" => |s| s.type_id,
                .@"union" => |u| u.type_id,
                else => return base_path,
            };
            const field_name_id = self.getFieldId(type_id, @intCast(sfv.field_index)) orelse return base_path;
            const field_name = self.getName(field_name_id);

            if (base_path) |bp| {
                const arena_alloc = self.error_name_arena.allocator();
                return std.fmt.allocPrint(arena_alloc, "{s}.{s}", .{ bp, field_name }) catch return field_name;
            }
            return field_name;
        },
        .load => |l| {
            // Load inherits name from its pointer source
            const ptr = switch (l.ptr) {
                .inst => |idx| idx,
                .interned, .fnptr => return null,
            };
            return self.buildPathName(results, refinements, ptr);
        },
        .arg => |a| {
            // Arg has its name in the tag
            return self.getName(a.name_id);
        },
        .optional_payload => |op| {
            // Optional unwrap: base.?
            const src_idx = switch (op.src) {
                .inst => |idx| idx,
                .interned, .fnptr => return null,
            };
            const base_path = self.buildPathName(results, refinements, src_idx) orelse return null;
            const arena_alloc = self.error_name_arena.allocator();
            return std.fmt.allocPrint(arena_alloc, "{s}.?", .{base_path}) catch return null;
        },
        else => return null,
    }
}

/// Create a shallow copy of the context (for branch execution).
/// All fields are copied by value - pointers/slices share underlying data.
pub fn copy(self: *Context) error{OutOfMemory}!*Context {
    const new_ctx = try self.allocator.create(Context);
    new_ctx.* = self.*;
    return new_ctx;
}

/// Delete a copied context.
pub fn delete(self: *Context) void {
    self.allocator.destroy(self);
}

pub fn push_fn(self: *Context, func_name: []const u8) !void {
    self.meta = .{
        .function = func_name,
        .file = self.meta.file,
        .line = 0,
        .column = null,
    };
    try self.prependTrace(self.meta);
}

pub fn pop_fn(self: *Context) void {
    const current = self.stacktrace.popFirst() orelse @panic("you busted the stacktrace");
    _ = current;
    if (self.stacktrace.at(0)) |caller| {
        self.meta = caller.data;
    } else {
        self.meta = .{
            .function = "",
            .file = "",
            .line = 0,
            .column = null,
        };
    }
}

pub fn setLocation(self: *Context, line: u32, column: u32) !void {
    self.meta.line = line;
    self.meta.column = column;

    const current = self.stacktrace.at(0) orelse {
        try self.prependTrace(self.meta);
        return;
    };
    const parent = current.node.next;
    const replacement = try self.trace_arena.allocator().create(Trace.Item);
    replacement.* = .{ .data = self.meta };
    replacement.node.next = parent;
    self.stacktrace.wrapped.first = &replacement.node;
}

pub fn captureTrace(self: *const Context) Trace {
    return self.stacktrace;
}

pub fn traceFromMeta(self: *Context, meta: core.Meta) error{OutOfMemory}!Trace {
    const frame = try self.trace_arena.allocator().create(Trace.Item);
    frame.* = .{ .data = meta };
    var trace: Trace = .{};
    trace.prepend(frame);
    return trace;
}

pub fn traceLeaf(trace: Trace) ?core.Meta {
    return if (trace.at(0)) |frame| frame.data else null;
}

pub fn printTrace(trace: Trace, writer: anytype, comptime prefix: []const u8, prefix_args: anytype) !void {
    const leaf = trace.at(0) orelse return;
    try leaf.data.print(writer, prefix, prefix_args);

    var caller = leaf.next();
    while (caller) |frame| : (caller = frame.next()) {
        try frame.data.print(writer, "called from ", .{});
    }
}

pub fn currentFunction(self: *const Context) []const u8 {
    return (self.stacktrace.at(0) orelse @panic("currentFunction called outside a function")).data.function;
}

pub fn traceDepth(self: *const Context) usize {
    return self.stacktrace.len();
}

pub fn restoreTrace(self: *Context, trace: Trace) void {
    self.stacktrace = trace;
    if (trace.at(0)) |frame| {
        self.meta = frame.data;
    }
}

pub fn restoreExecutionPoint(self: *Context, meta: core.Meta, trace: Trace) void {
    self.meta = meta;
    self.stacktrace = trace;
}

fn prependTrace(self: *Context, meta: core.Meta) !void {
    const frame = try self.trace_arena.allocator().create(Trace.Item);
    frame.* = .{ .data = meta };
    self.stacktrace.prepend(frame);
}

pub fn dumpStackTrace(self: *Context) void {
    self.writer.writeAll("Stack trace:\n") catch {};
    var frame = self.stacktrace.at(0);
    while (frame) |item| : (frame = item.next()) {
        item.data.print(self.writer, "  ", .{}) catch {};
    }
}

test "context stacktrace tracks calls" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    try ctx.push_fn("first");
    try ctx.push_fn("second");

    try std.testing.expectEqual(@as(usize, 2), ctx.stacktrace.len());
    try std.testing.expectEqualStrings("second", ctx.stacktrace.at(0).?.data.function);
    try std.testing.expectEqualStrings("first", ctx.stacktrace.at(1).?.data.function);

    ctx.pop_fn();
    try std.testing.expectEqual(@as(usize, 1), ctx.stacktrace.len());
}

test "pop_fn restores meta.function to caller" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    // Push first function
    try ctx.push_fn("caller_func");
    try std.testing.expectEqualStrings("caller_func", ctx.meta.function);

    // Push second function
    try ctx.push_fn("callee_func");
    try std.testing.expectEqualStrings("callee_func", ctx.meta.function);

    // Pop callee - meta.function should restore to caller
    ctx.pop_fn();
    try std.testing.expectEqualStrings("caller_func", ctx.meta.function);

    // Pop caller - meta.function should be empty
    ctx.pop_fn();
    try std.testing.expectEqualStrings("", ctx.meta.function);
}

test "stacktrace preserves full caller metadata across push and pop" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    ctx.meta.file = "caller.zig";
    try ctx.push_fn("caller");
    try ctx.setLocation(12, 7);

    ctx.meta.file = "callee.zig";
    try ctx.push_fn("callee");
    try ctx.setLocation(30, 9);

    try std.testing.expectEqualStrings("callee", ctx.stacktrace.at(0).?.data.function);
    try std.testing.expectEqualStrings("callee.zig", ctx.stacktrace.at(0).?.data.file);
    try std.testing.expectEqual(@as(u32, 30), ctx.stacktrace.at(0).?.data.line);
    try std.testing.expectEqualStrings("caller", ctx.stacktrace.at(1).?.data.function);
    try std.testing.expectEqualStrings("caller.zig", ctx.stacktrace.at(1).?.data.file);
    try std.testing.expectEqual(@as(u32, 12), ctx.stacktrace.at(1).?.data.line);

    ctx.pop_fn();
    try std.testing.expectEqualStrings("caller", ctx.meta.function);
    try std.testing.expectEqualStrings("caller.zig", ctx.meta.file);
    try std.testing.expectEqual(@as(u32, 12), ctx.meta.line);
    try std.testing.expectEqual(@as(u32, 7), ctx.meta.column.?);
}

test "stacktrace location replacement leaves captured trace immutable" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    ctx.meta.file = "main.zig";
    try ctx.push_fn("main");
    try ctx.setLocation(4, 2);
    const captured = ctx.captureTrace();

    try ctx.setLocation(8, 6);

    try std.testing.expectEqual(@as(u32, 4), captured.at(0).?.data.line);
    try std.testing.expectEqual(@as(u32, 2), captured.at(0).?.data.column.?);
    try std.testing.expectEqual(@as(u32, 8), ctx.stacktrace.at(0).?.data.line);
    try std.testing.expectEqual(@as(u32, 6), ctx.stacktrace.at(0).?.data.column.?);
}

test "printTrace renders leaf followed by callers" {
    var output: [4096]u8 = undefined;
    var writer = std.Io.Writer.fixed(&output);
    var ctx = Context.init(std.testing.allocator, &writer);
    defer ctx.deinit();

    ctx.meta.file = "caller.zig";
    try ctx.push_fn("caller");
    try ctx.setLocation(10, 4);
    ctx.meta.file = "callee.zig";
    try ctx.push_fn("callee");
    try ctx.setLocation(20, 8);

    try Context.printTrace(ctx.captureTrace(), &writer, "event in ", .{});

    try std.testing.expectEqualStrings(
        "event in callee (callee.zig:20:8)\n" ++
            "called from caller (caller.zig:10:4)\n",
        writer.buffered(),
    );
}

// Test helper: getName that maps specific IDs to names
fn pathTestGetName(id: u32) []const u8 {
    return switch (id) {
        1 => "foo",
        2 => "bar",
        3 => "baz",
        4 => "opt",
        else => "unknown",
    };
}

// Test helper: getFieldId that maps (type_id, field_index) to name_id
fn pathTestGetFieldId(type_id: u32, field_index: u32) ?u32 {
    // Type 100 has fields: bar (id=2), baz (id=3)
    if (type_id == 100) {
        return switch (field_index) {
            0 => 2, // bar
            1 => 3, // baz
            else => null,
        };
    }
    return null;
}

test "buildPathName returns name from inst.name_id" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Inst with name_id set directly (e.g., from dbg_var_ptr)
    var results = [_]Inst{.{ .name_id = 1 }};

    const path = ctx.buildPathName(&results, &refinements, 0);
    try std.testing.expectEqualStrings("foo", path.?);
}

test "buildPathName returns name from arg tag" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Inst with arg tag containing name_id
    // Arg.value is now a Gid - use 0 as placeholder since we just need the name_id
    var results = [_]Inst{.{ .inst_tag = .{ .arg = .{ .value = 0, .name_id = 2 } } }};

    const path = ctx.buildPathName(&results, &refinements, 0);
    try std.testing.expectEqualStrings("bar", path.?);
}

test "buildPathName for load inherits from pointer source" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Inst 0: named variable "foo"
    // Inst 1: load from inst 0
    var results = [_]Inst{
        .{ .name_id = 1 }, // foo
        .{ .inst_tag = .{ .load = .{ .ptr = .{ .inst = 0 } } } },
    };

    const path = ctx.buildPathName(&results, &refinements, 1);
    try std.testing.expectEqualStrings("foo", path.?);
}

test "buildPathName for optional_payload appends .?" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Inst 0: named variable "opt"
    // Inst 1: optional_payload unwrap of inst 0
    var results = [_]Inst{
        .{ .name_id = 4 }, // opt
        .{ .inst_tag = .{ .optional_payload = .{ .src = .{ .inst = 0 } } } },
    };

    const path = ctx.buildPathName(&results, &refinements, 1);
    try std.testing.expectEqualStrings("opt.?", path.?);
}

test "buildPathName for nested optional unwrap shows chain" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Inst 0: named variable "opt"
    // Inst 1: optional_payload unwrap of inst 0 -> opt.?
    // Inst 2: optional_payload unwrap of inst 1 -> opt.?.?
    var results = [_]Inst{
        .{ .name_id = 4 }, // opt
        .{ .inst_tag = .{ .optional_payload = .{ .src = .{ .inst = 0 } } } },
        .{ .inst_tag = .{ .optional_payload = .{ .src = .{ .inst = 1 } } } },
    };

    const path = ctx.buildPathName(&results, &refinements, 2);
    try std.testing.expectEqualStrings("opt.?.?", path.?);
}

test "buildPathName for struct_field_ptr builds field path" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;
    ctx.getFieldId = &pathTestGetFieldId;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Create a pointer to struct refinement for inst 0
    // The struct has type_id = 100
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .type_id = 100, .fields = &.{} } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .info = .{ .to = struct_gid } } });

    // Inst 0: named variable "foo" pointing to struct
    // Inst 1: struct_field_ptr accessing field 0 (bar) of inst 0
    var results = [_]Inst{
        .{ .name_id = 1, .refinement = ptr_gid }, // foo
        .{ .inst_tag = .{ .struct_field_ptr = .{ .base = .{ .inst = 0 }, .field_index = 0, .ty = .{ .scalar = .{} } } } },
    };

    const path = ctx.buildPathName(&results, &refinements, 1);
    try std.testing.expectEqualStrings("foo.bar", path.?);
}

test "buildPathName for compound path: foo.?.bar" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();
    ctx.getName = &pathTestGetName;
    ctx.getFieldId = &pathTestGetFieldId;

    var refinements = Refinements.init(std.testing.allocator);
    defer refinements.deinit();

    // Create refinements for the path: foo (optional containing ptr to struct)
    // When unwrapped, gives pointer to struct with type_id = 100
    const struct_gid = try refinements.appendEntity(.{ .@"struct" = .{ .type_id = 100, .fields = &.{} } });
    const ptr_gid = try refinements.appendEntity(.{ .pointer = .{ .info = .{ .to = struct_gid } } });

    // Inst 0: named variable "foo" (the optional)
    // Inst 1: optional_payload of inst 0 -> foo.? (gives ptr to struct)
    // Inst 2: struct_field_ptr of inst 1, field 0 -> foo.?.bar
    var results = [_]Inst{
        .{ .name_id = 1 }, // foo
        .{ .inst_tag = .{ .optional_payload = .{ .src = .{ .inst = 0 } } }, .refinement = ptr_gid },
        .{ .inst_tag = .{ .struct_field_ptr = .{ .base = .{ .inst = 1 }, .field_index = 0, .ty = .{ .scalar = .{} } } } },
    };

    const path = ctx.buildPathName(&results, &refinements, 2);
    try std.testing.expectEqualStrings("foo.?.bar", path.?);
}
