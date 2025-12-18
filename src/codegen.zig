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
fn payload(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, _: usize, extra: []const u32, param_names: []const []const u8, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .arg => payloadArg(arena, datum, param_names, arg_counter),
        .dbg_stmt => payloadDbgStmt(arena, datum),
        .store_safe => payloadStoreSafe(arena, ip, datum),
        .load => payloadLoad(arena, datum),
        .ret_safe => payloadRetSafe(arena, datum),
        .dbg_var_ptr, .dbg_var_val, .dbg_arg_inline => payloadDbgVar(arena, datum, extra),
        .bitcast, .unwrap_errunion_payload => payloadTransferOp(arena, datum),
        .br => payloadBr(arena, datum),
        else => ".{}",
    };
}

/// Payload for operations that transfer all properties from source to result.
/// Used for bitcast, unwrap_errunion_payload, etc. where the result carries
/// the same memory safety / allocation state as the source.
fn payloadTransferOp(arena: std.mem.Allocator, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src: ?usize = if (operand.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(arena, ".{{ .src = {?d} }}", .{src}, null);
}

/// Payload for br (branch to block with value).
/// Transfers properties from operand to target block.
fn payloadBr(arena: std.mem.Allocator, datum: Data) []const u8 {
    const block_inst = @intFromEnum(datum.br.block_inst);
    const src: ?usize = if (datum.br.operand.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(arena, ".{{ .block = {d}, .src = {?d} }}", .{ block_inst, src }, null);
}


/// Generate a single slot line for a given tag and data.
/// Exposed for testing with underscore prefix to indicate internal use.
/// arg_counter tracks sequential arg indices (may differ from zir_param_index).
pub fn _slotLine(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, inst_index: usize, extra: []const u32, tags: []const Tag, data: []const Data, param_names: []const []const u8, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .call, .call_always_tail, .call_never_tail, .call_never_inline => blk: {
            if (isDebugCall(ip, datum)) {
                break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .noop_pruned_debug = .{{}} }}, tracked, {d}, ctx);\n", .{inst_index}, null);
            }
            if (isAllocatorCreate(ip, datum)) {
                // Prune allocator.create() - emit special tag for tracking
                break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .alloc_create = .{{}} }}, tracked, {d}, ctx);\n", .{inst_index}, null);
            }
            if (isAllocatorDestroy(ip, datum)) {
                // Prune allocator.destroy() - emit special tag with pointer slot
                const ptr_slot = extractDestroyPtrSlot(datum, extra, tags, data);
                break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .alloc_destroy = .{{ .ptr = {?d} }} }}, tracked, {d}, ctx);\n", .{ ptr_slot, inst_index }, null);
            }
            const call_parts = payloadCallParts(arena, ip, datum, extra, tags, data);
            break :blk clr_allocator.allocPrint(arena, "    try Slot.call({s}, {s}, tracked, {d}, ctx);\n", .{ call_parts.called, call_parts.args, inst_index }, null);
        },
        else => blk: {
            const tag_payload = payload(arena, ip, tag, datum, inst_index, extra, param_names, arg_counter);
            break :blk clr_allocator.allocPrint(arena, "    try Slot.apply(.{{ .{s} = {s} }}, tracked, {d}, ctx);\n", .{ safeName(tag), tag_payload, inst_index }, null);
        },
    };
}

/// Generate payload for .arg tag.
///
/// zir_param_index is the original ZIR parameter index, which may have gaps when
/// comptime parameters are present (they don't become runtime args but keep their
/// ZIR indices). For example, a function with 4 ZIR params where param 1 is comptime
/// would have zir_param_index values of 0, 2, 3 for its 3 runtime .arg tags.
///
/// We use zir_param_index for name lookup (param_names is indexed by ZIR index),
/// but arg_counter for the argN reference (sequential: arg0, arg1, arg2).
///
/// arg_counter is optional to support unit tests that test single tags in isolation
/// without needing to set up a counter. When null, falls back to zir_param_index
/// (works for tests since they typically use index 0).
fn payloadArg(arena: std.mem.Allocator, datum: Data, param_names: []const []const u8, arg_counter: ?*u32) []const u8 {
    const zir_param_index = datum.arg.zir_param_index;
    const name = if (zir_param_index < param_names.len) param_names[zir_param_index] else "";
    const arg_index = if (arg_counter) |counter| blk: {
        const idx = counter.*;
        counter.* += 1;
        break :blk idx;
    } else zir_param_index;
    return clr_allocator.allocPrint(arena, ".{{ .value = arg{d}, .name = \"{s}\" }}", .{ arg_index, name }, null);
}

fn payloadDbgStmt(arena: std.mem.Allocator, datum: Data) []const u8 {
    return clr_allocator.allocPrint(arena, ".{{ .line = {d}, .column = {d} }}", .{
        datum.dbg_stmt.line,
        datum.dbg_stmt.column,
    }, null);
}

fn payloadStoreSafe(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    // bin_op: lhs = destination ptr, rhs = source value
    const is_undef = isUndefRef(ip, datum.bin_op.rhs);
    const ptr: ?usize = if (datum.bin_op.lhs.toIndex()) |idx| @intFromEnum(idx) else null;
    const src: ?usize = if (datum.bin_op.rhs.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(arena, ".{{ .ptr = {?d}, .src = {?d}, .is_undef = {} }}", .{ ptr, src, is_undef }, null);
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

fn payloadDbgVar(arena: std.mem.Allocator, datum: Data, extra: []const u32) []const u8 {
    const operand = datum.pl_op.operand;
    const name_index = datum.pl_op.payload;

    // Extract the slot being named
    const slot: ?usize = if (operand.toIndex()) |idx| @intFromEnum(idx) else null;

    // Extract the variable name from extra as NullTerminatedString
    const name = extractString(extra, name_index);

    return clr_allocator.allocPrint(arena, ".{{ .slot = {?d}, .name = \"{s}\" }}", .{ slot, name }, null);
}

fn extractString(extra: []const u32, start: u32) []const u8 {
    // Extra stores strings as sequences of u32s containing packed bytes
    const bytes: [*]const u8 = @ptrCast(extra[start..].ptr);
    // Find null terminator
    var len: usize = 0;
    while (bytes[len] != 0) : (len += 1) {}
    return bytes[0..len];
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
            // Use @constCast since constants don't need state propagation back
            if (first) {
                args_str = clr_allocator.allocPrint(arena, "{s} @constCast(&Slot{{}})", .{args_str}, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(arena, "{s}, @constCast(&Slot{{}})", .{args_str}, null);
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
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isDebugFqn(fqn);
}

/// Check if an interned function reference is an Allocator.create call
fn isAllocatorCreate(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorCreateFqn(fqn);
}

/// Check if an interned function reference is an Allocator.destroy call
fn isAllocatorDestroy(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorDestroyFqn(fqn);
}

/// Extract FQN from a call instruction's callee reference
fn getCallFqn(ip: *const InternPool, datum: Data) ?[]const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return null;

    const key = ip.indexToKey(ip_idx);
    switch (key) {
        .func => |func_key| {
            const nav = ip.getNav(func_key.owner_nav);
            return nav.fqn.toSlice(ip);
        },
        else => return null,
    }
}

/// Check if FQN is a debug.* function (testable without InternPool)
pub fn isDebugFqn(fqn: []const u8) bool {
    return std.mem.startsWith(u8, fqn, "debug.");
}

/// Check if FQN is an Allocator.create call (testable without InternPool)
/// Matches patterns like "mem.Allocator.create__anon_*" or "std.mem.Allocator.create__anon_*"
pub fn isAllocatorCreateFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.create") != null;
}

/// Check if FQN is an Allocator.destroy call (testable without InternPool)
/// Matches patterns like "mem.Allocator.destroy__anon_*" or "std.mem.Allocator.destroy__anon_*"
pub fn isAllocatorDestroyFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.destroy") != null;
}

/// Extract the pointer slot being destroyed from a destroy call.
/// destroy(self, ptr) - the second argument (index 1) is the pointer.
fn extractDestroyPtrSlot(datum: Data, extra: []const u32, tags: []const Tag, data: []const Data) ?usize {
    _ = tags;
    _ = data;
    const payload_index = datum.pl_op.payload;
    const args_len = extra[payload_index];

    // destroy has 2 args: (self, ptr) - we want arg index 1
    if (args_len < 2) return null;

    const ptr_arg_ref: Ref = @enumFromInt(extra[payload_index + 1 + 1]); // +1 for args_len, +1 for second arg
    if (ptr_arg_ref.toIndex()) |idx| {
        return @intFromEnum(idx);
    }
    return null;
}

/// Build all slot apply lines into a single buffer (uses provided arena allocator)
fn buildSlotLines(arena: std.mem.Allocator, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32, param_names: []const []const u8) []const u8 {
    if (tags.len == 0) return "";

    // Build lines into a list first
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;

    // Track sequential arg counter (separate from zir_param_index)
    var arg_counter: u32 = 0;

    for (tags, data, 0..) |tag, datum, i| {
        const line = _slotLine(arena, ip, tag, datum, i, extra, tags, data, param_names, &arg_counter);
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
pub fn generateFunction(func_index: u32, fqn: []const u8, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32, base_line: u32, file_path: []const u8, param_names: []const []const u8) []u8 {
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
    const slot_lines = buildSlotLines(arena.allocator(), ip, tags, data, extra, param_names);

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
        \\{s}    try slots.onFinish(tracked, &retval, ctx);
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
