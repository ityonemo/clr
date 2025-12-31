const std = @import("std");
const tag = @import("tag.zig");
const Refinements = @import("Refinements.zig");
const Inst = @import("Inst.zig");

allocator: std.mem.Allocator,
stacktrace: std.ArrayListUnmanaged([]const u8),
meta: @import("Meta.zig"),
base_line: u32 = 0,
writer: *std.Io.Writer = undefined,

/// Name lookup function pointer - set by generated .air.zig main()
/// Converts a Name ID (u32) to a string slice
getName: *const fn (u32) []const u8 = undefined,

/// Field name lookup function pointer - set by generated .air.zig main()
/// Converts (type_id, field_index) to a name_id for getName
/// Returns null for tuple types or unknown fields
getFieldId: *const fn (u32, u32) ?u32 = undefined,

/// Arena for error path names - strings allocated here are freed all at once at end of run
error_name_arena: std.heap.ArenaAllocator,

const Context = @This();

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
        .stacktrace = .empty,
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
    };
}

pub fn deinit(self: *Context) void {
    self.stacktrace.deinit(self.allocator);
    self.error_name_arena.deinit();
}

/// Build a full access path name for an instruction by walking the tag chain.
/// Allocated from error_name_arena. Returns null if no name can be determined.
pub fn buildPathName(self: *Context, results: []const Inst, refinements: *Refinements, index: usize) ?[]const u8 {
    const inst = results[index];

    // Check for root variable name (set by dbg_var_ptr/dbg_var_val)
    if (inst.name_id) |name_id| {
        return self.getName(name_id);
    }

    // Walk the tag chain to build compound paths
    const t = inst.inst_tag orelse return null;
    switch (t) {
        .struct_field_ptr => |sfp| {
            const base = sfp.base orelse return null;
            const base_path = self.buildPathName(results, refinements, base);

            // Get field name from container's type_id
            const base_ref_idx = results[base].refinement orelse return base_path;
            const type_id = switch (refinements.at(base_ref_idx).*) {
                .pointer => |p| blk: {
                    // struct_field_ptr's base is a pointer to struct/union
                    const pointee = refinements.at(p.to);
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
            const ptr = l.ptr orelse return null;
            return self.buildPathName(results, refinements, ptr);
        },
        .arg => |a| {
            // Arg has its name in the tag
            return self.getName(a.name_id);
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
    self.meta.function = func_name;
    try self.stacktrace.append(self.allocator, func_name);
}

pub fn pop_fn(self: *Context) void {
    if (self.stacktrace.items.len == 0) @panic("you busted the stacktrace");
    _ = self.stacktrace.pop();
    // Restore meta.function to the caller's function name
    if (self.stacktrace.items.len > 0) {
        self.meta.function = self.stacktrace.items[self.stacktrace.items.len - 1];
    } else {
        self.meta.function = "";
    }
}

pub fn dumpStackTrace(self: *Context) void {
    var buf: [1024]u8 = undefined;
    const rel_path = std.fs.path.relative(self.allocator, std.fs.cwd().realpathAlloc(self.allocator, ".") catch ".", self.meta.file) catch self.meta.file;
    self.writer.writeAll("Stack trace:\n") catch {};
    // Print frames in reverse order (most recent first)
    var i = self.stacktrace.items.len;
    while (i > 0) {
        i -= 1;
        const frame = self.stacktrace.items[i];
        if (i == self.stacktrace.items.len - 1) {
            // Most recent frame - include file/line info
            const msg = std.fmt.bufPrint(&buf, "  {s} ({s}:{d}:{d})\n", .{ frame, rel_path, self.meta.line, self.meta.column orelse 0 }) catch continue;
            self.writer.writeAll(msg) catch {};
        } else {
            const msg = std.fmt.bufPrint(&buf, "  {s}\n", .{frame}) catch continue;
            self.writer.writeAll(msg) catch {};
        }
    }
}

test "context stacktrace tracks calls" {
    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(std.testing.allocator, &discarding.writer);
    defer ctx.deinit();

    try ctx.push_fn("first");
    try ctx.push_fn("second");

    try std.testing.expectEqual(@as(usize, 2), ctx.stacktrace.items.len);
    try std.testing.expectEqualStrings("first", ctx.stacktrace.items[0]);
    try std.testing.expectEqualStrings("second", ctx.stacktrace.items[1]);

    ctx.pop_fn();
    try std.testing.expectEqual(@as(usize, 1), ctx.stacktrace.items.len);
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
