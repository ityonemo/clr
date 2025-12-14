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

/// Returns the payload string for a given tag and data.
/// Note: call tags are handled separately in buildSlotLines via payloadCallParts.
fn payload(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, inst_index: usize) []const u8 {
    return switch (tag) {
        .arg => payloadArg(arena, inst_index),
        .dbg_stmt => payloadDbgStmt(arena, datum),
        .store_safe => payloadStoreSafe(arena, ip, datum),
        .load => payloadLoad(arena, datum),
        .ret_safe => payloadRetSafe(arena, datum),
        else => ".{}",
    };
}

/// Generate a single slot line for a given tag and data.
/// Exposed for testing with underscore prefix to indicate internal use.
pub fn _slotLine(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, inst_index: usize, extra: []const u32, tags: []const Tag, data: []const Data) []const u8 {
    return switch (tag) {
        .call, .call_always_tail, .call_never_tail, .call_never_inline => blk: {
            if (isDebugCall(ip, datum)) {
                break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .noop_pruned_debug = .{{}} }}, tracked, {d}, ctx);\n", .{inst_index}, null);
            }
            const call_parts = payloadCallParts(arena, ip, datum, extra, tags, data);
            break :blk clr_allocator.allocPrint(arena, "    try Slot.call({s}, {s}, tracked, {d}, ctx);\n", .{ call_parts.called, call_parts.args, inst_index }, null);
        },
        else => blk: {
            const tag_payload = payload(arena, ip, tag, datum, inst_index);
            break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .{s} = {s} }}, tracked, {d}, ctx);\n", .{ safeName(tag), tag_payload, inst_index }, null);
        },
    };
}

fn payloadArg(arena: std.mem.Allocator, inst_index: usize) []const u8 {
    return clr_allocator.allocPrint(arena, ".{{ .value = arg{d} }}", .{inst_index}, null);
}

fn payloadDbgStmt(arena: std.mem.Allocator, datum: Data) []const u8 {
    return clr_allocator.allocPrint(arena, ".{{ .line = {d}, .column = {d} }}", .{
        datum.dbg_stmt.line,
        datum.dbg_stmt.column,
    }, null);
}

fn payloadStoreSafe(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    // TODO: handle global/interned pointers - for now emit null
    const is_undef = isUndefRef(ip, datum.bin_op.rhs);
    if (datum.bin_op.lhs.toIndex()) |idx| {
        const ptr = @intFromEnum(idx);
        return clr_allocator.allocPrint(arena, ".{{ .ptr = {d}, .is_undef = {} }}", .{ ptr, is_undef }, null);
    } else {
        return clr_allocator.allocPrint(arena, ".{{ .ptr = null, .is_undef = {} }}", .{is_undef}, null);
    }
}

fn payloadLoad(arena: std.mem.Allocator, datum: Data) []const u8 {
    // TODO: handle global/interned pointers - for now emit null
    if (datum.ty_op.operand.toIndex()) |idx| {
        const ptr = @intFromEnum(idx);
        return clr_allocator.allocPrint(arena, ".{{ .ptr = {d} }}", .{ptr}, null);
    } else {
        return ".{ .ptr = null }";
    }
}

fn payloadRetSafe(arena: std.mem.Allocator, datum: Data) []const u8 {
    const operand_index = datum.un_op.toIndex() orelse return ".{ .retval_ptr = &retval, .src = null }";
    const ptr = @intFromEnum(operand_index);
    return clr_allocator.allocPrint(arena, ".{{ .retval_ptr = &retval, .src = {d} }}", .{ptr}, null);
}

const CallParts = struct {
    called: []const u8,
    args: []const u8,
};

fn payloadCallParts(arena: std.mem.Allocator, ip: *const InternPool, datum: Data, extra: []const u32, tags: []const Tag, data: []const Data) CallParts {
    _ = data; // TODO: may need for resolving callee
    // Call uses pl_op: operand is callee, payload indexes into extra
    // extra[payload] is args_len, followed by args_len Refs
    const callee_ref = datum.pl_op.operand;
    const payload_index = datum.pl_op.payload;
    const args_len = extra[payload_index];

    // Get called function - look up the instruction that produces the callee
    const called_str = if (callee_ref.toIndex()) |idx| blk: {
        // Indirect call through function pointer (load, slice_elem_val, etc.)
        const callee_idx = @intFromEnum(idx);
        const callee_tag = tags[callee_idx];
        _ = callee_tag;
        break :blk "null"; // TODO: handle indirect calls
    } else if (callee_ref.toInterned()) |ip_idx| blk: {
        // Direct call to known function - use InternPool index as func_index
        break :blk clr_allocator.allocPrint(arena, "fn_{d}", .{@intFromEnum(ip_idx)}, null);
    } else "null";

    // Build args tuple string: .{ &tracked[arg0], &tracked[arg1], ... }
    var args_str: []const u8 = ".{";
    var first = true;

    var i: u32 = 0;
    while (i < args_len) : (i += 1) {
        const arg_ref: Ref = @enumFromInt(extra[payload_index + 1 + i]);
        if (arg_ref.toIndex()) |idx| {
            // Runtime value from local instruction - pass pointer to slot
            const slot_idx = @intFromEnum(idx);
            if (first) {
                args_str = clr_allocator.allocPrint(arena, "{s} &tracked[{d}]", .{ args_str, slot_idx }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(arena, "{s}, &tracked[{d}]", .{ args_str, slot_idx }, null);
            }
        } else if (arg_ref.toInterned()) |interned_idx| {
            // Skip zero-sized types - they have no runtime representation
            const val_type = ip.typeOf(interned_idx);
            if (isZeroSizedType(ip, val_type)) continue;
            // Runtime global/constant - pass pointer to empty Slot
            // TODO: This creates a temporary; may need better handling for constants
            if (first) {
                args_str = clr_allocator.allocPrint(arena, "{s} &Slot{{}}", .{args_str}, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(arena, "{s}, &Slot{{}}", .{args_str}, null);
            }
        }
    }
    args_str = clr_allocator.allocPrint(arena, "{s} }}", .{args_str}, null);

    return .{ .called = called_str, .args = args_str };
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

/// Check if a type is zero-sized (has no runtime representation).
/// Zero-sized types in Zig:
/// - void
/// - u0 and i0
/// - Arrays/vectors with len 0 or zero-bit element type
/// - Enums with only 1 tag
/// - Structs with all zero-bit fields
/// - Unions with only 1 field which is zero-bit
fn isZeroSizedType(ip: *const InternPool, type_index: InternPool.Index) bool {
    const type_key = ip.indexToKey(type_index);
    return switch (type_key) {
        .simple_type => |simple| simple == .void,
        .int_type => |int_info| int_info.bits == 0,
        .array_type => |array_info| array_info.len == 0 or isZeroSizedType(ip, array_info.child),
        .vector_type => |vector_info| vector_info.len == 0 or isZeroSizedType(ip, vector_info.child),
        .struct_type => blk: {
            const loaded = ip.loadStructType(type_index);
            if (loaded.field_types.len == 0) break :blk true;
            // Check if all fields are zero-sized
            for (loaded.field_types.get(ip)) |field_type| {
                if (!isZeroSizedType(ip, field_type)) break :blk false;
            }
            break :blk true;
        },
        .enum_type => blk: {
            const loaded = ip.loadEnumType(type_index);
            break :blk loaded.names.len == 1;
        },
        .union_type => blk: {
            const loaded = ip.loadUnionType(type_index);
            if (loaded.field_types.len != 1) break :blk false;
            break :blk isZeroSizedType(ip, loaded.field_types.get(ip)[0]);
        },
        else => false,
    };
}

/// Count the number of arg instructions (function parameters)
fn countArgs(tags: []const Tag) u32 {
    var count: u32 = 0;
    for (tags) |tag| {
        if (tag == .arg) count += 1;
    }
    return count;
}

/// Generate parameter list string like "arg0: *Slot, arg1: *Slot"
fn buildParamList(arena: std.mem.Allocator, arg_count: u32) []const u8 {
    if (arg_count == 0) return "";

    var result: []const u8 = clr_allocator.allocPrint(arena, "arg0: *Slot", .{}, null);
    var i: u32 = 1;
    while (i < arg_count) : (i += 1) {
        result = clr_allocator.allocPrint(arena, "{s}, arg{d}: *Slot", .{ result, i }, null);
    }
    return result;
}

/// Check if an interned function reference is a debug.* function that should be pruned
fn isDebugCall(ip: *const InternPool, datum: Data) bool {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return false;

    // Check if this is actually a function in the InternPool
    const key = ip.indexToKey(ip_idx);
    switch (key) {
        .func => |func_key| {
            const nav = ip.getNav(func_key.owner_nav);
            const callee_fqn = nav.fqn.toSlice(ip);
            return std.mem.startsWith(u8, callee_fqn, "debug.");
        },
        else => return false,
    }
}

/// Build all slot apply lines into a single buffer (uses provided arena allocator)
fn buildSlotLines(arena: std.mem.Allocator, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32) []const u8 {
    if (tags.len == 0) return "";

    // Build lines into a list first
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;

    for (tags, data, 0..) |tag, datum, i| {
        const line = _slotLine(arena, ip, tag, datum, i, extra, tags, data);
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
pub fn generateFunction(func_index: u32, fqn: []const u8, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32, base_line: u32, file_path: []const u8) []u8 {
    if (tags.len == 0) @panic("function with no instructions encountered");

    // Per-function arena for temporary allocations
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Count args and build parameter list
    const arg_count = countArgs(tags);
    const param_list = buildParamList(arena.allocator(), arg_count);
    const params: []const u8 = if (arg_count > 0)
        clr_allocator.allocPrint(arena.allocator(), ", {s}", .{param_list}, null)
    else
        "";

    // Build slot lines first
    const slot_lines = buildSlotLines(arena.allocator(), ip, tags, data, extra);

    // Generate complete function with slot lines injected (use main arena for final result)
    // Size hint: slot_lines + template + margin
    const size_hint = slot_lines.len + 1024;
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\fn fn_{d}(ctx: *Context{s}) anyerror!Slot {{
        \\    ctx.file = "{s}";
        \\    ctx.base_line = {d};
        \\    try ctx.push("{s}");
        \\    defer ctx.pop();
        \\
        \\    const tracked = slots.make_list(ctx.allocator, {d});
        \\    defer slots.clear_list(tracked, ctx.allocator);
        \\    var retval: Slot = .{{}};
        \\
        \\{s}    retval = retval;
        \\    return retval;
        \\}}
        \\
    , .{ func_index, params, file_path, base_line, fqn, tags.len, slot_lines }, size_hint);
}

/// Generate epilogue with imports and main function
pub fn epilogue(entrypoint_index: u32) []u8 {
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const slots = clr.slots;
        \\const Slot = slots.Slot;
        \\
        \\pub fn main() void {{
        \\    var gpa = std.heap.GeneralPurposeAllocator(.{{}}){{}};
        \\    defer _ = gpa.deinit();
        \\    const allocator = gpa.allocator();
        \\    var ctx = Context.init(allocator);
        \\    defer ctx.deinit();
        \\    _ = fn_{d}(&ctx) catch std.process.exit(1);
        \\}}
        \\
    , .{entrypoint_index}, null);
}

/// Generate a stub function for a missing call target
pub fn generateStub(func_index: u32, arity: u32) []u8 {
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Build parameter list: ctx + arity *Slot args
    var params: []const u8 = "ctx: *Context";
    var i: u32 = 0;
    while (i < arity) : (i += 1) {
        params = clr_allocator.allocPrint(arena.allocator(), "{s}, _: *Slot", .{params}, null);
    }

    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\fn fn_{d}({s}) anyerror!Slot {{
        \\    std.debug.print("WARNING: call to unresolved function fn_{d}\\n", .{{}});
        \\    ctx.dumpStackTrace();
        \\    return .{{}};
        \\}}
        \\
    , .{ func_index, params, func_index }, null);
}

const CallTarget = @import("clr.zig").CallTarget;

/// Extract all call targets (InternPool indices and arities) from AIR instructions
/// Skips calls to debug.* functions since they are pruned
pub fn extractCallTargets(allocator: std.mem.Allocator, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32) []CallTarget {
    var targets = std.ArrayListUnmanaged(CallTarget){};

    for (tags, data) |tag, datum| {
        switch (tag) {
            .call, .call_always_tail, .call_never_tail, .call_never_inline => {
                // Skip debug.* calls - they are pruned
                if (isDebugCall(ip, datum)) continue;

                const callee_ref = datum.pl_op.operand;
                const payload_index = datum.pl_op.payload;
                const args_len = extra[payload_index];
                if (callee_ref.toInterned()) |ip_idx| {
                    targets.append(allocator, .{
                        .index = @intFromEnum(ip_idx),
                        .arity = args_len,
                    }) catch continue;
                }
            },
            else => {},
        }
    }

    return targets.toOwnedSlice(allocator) catch &.{};
}

test {
    @import("std").testing.refAllDecls(@import("codegen_test.zig"));
}
