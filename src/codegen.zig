const clr_allocator = @import("allocator.zig");
const debug = @import("debug.zig");
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
/// Note: call tags are handled separately in buildInstLines via payloadCallParts.
fn payload(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, _: usize, extra: []const u32, param_names: []const []const u8, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .arg => payloadArg(arena, datum, param_names, arg_counter),
        .dbg_stmt => payloadDbgStmt(arena, datum),
        .store, .store_safe => payloadStore(arena, ip, datum),
        .load => payloadLoad(arena, datum),
        .ret_safe => payloadRetSafe2(arena, ip, datum),
        .dbg_var_ptr, .dbg_var_val, .dbg_arg_inline => payloadDbg(arena, datum, extra),
        .bitcast, .unwrap_errunion_payload, .optional_payload => payloadTransferOp(arena, ip, datum),
        .br => payloadBr(arena, ip, datum),
        else => ".{}",
    };
}

/// Payload for operations that transfer all properties from source to result.
/// Used for bitcast, unwrap_errunion_payload, etc. where the result carries
/// the same memory safety / allocation state as the source.
fn payloadTransferOp(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(arena, ip, operand);
    return clr_allocator.allocPrint(arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for ret_safe - just the src, caller_refinements and return_eidx come from State.
fn payloadRetSafe2(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    const src_str = srcString(arena, ip, datum.un_op);
    return clr_allocator.allocPrint(arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for br (branch to block with value).
/// Transfers properties from operand to target block.
fn payloadBr(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    const block_inst = @intFromEnum(datum.br.block_inst);
    const operand = datum.br.operand;
    const src_str = srcString(arena, ip, operand);
    return clr_allocator.allocPrint(arena, ".{{ .block = {d}, .src = {s} }}", .{ block_inst, src_str }, null);
}

/// Generate a single inst line for a given tag and data.
/// Exposed for testing with underscore prefix to indicate internal use.
/// arg_counter tracks sequential arg indices (may differ from zir_param_index).
pub fn _instLine(arena: std.mem.Allocator, ip: *const InternPool, tag: Tag, datum: Data, inst_index: usize, extra: []const u32, tags: []const Tag, data: []const Data, param_names: []const []const u8, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .call, .call_always_tail, .call_never_tail, .call_never_inline => blk: {
            if (isDebugCall(ip, datum)) {
                break :blk clr_allocator.allocPrint(arena, "    try Inst.apply(state, {d}, .{{ .noop_pruned_debug = .{{}} }});\n", .{inst_index}, null);
            }
            if (isAllocatorCreate(ip, datum)) {
                // Prune allocator.create() - emit special tag for tracking
                const allocator_type = extractAllocatorType(ip, datum, extra, tags, data);
                break :blk clr_allocator.allocPrint(arena, "    try Inst.apply(state, {d}, .{{ .alloc_create = .{{ .allocator_type = \"{s}\" }} }});\n", .{ inst_index, allocator_type }, null);
            }
            if (isAllocatorDestroy(ip, datum)) {
                // Prune allocator.destroy() - emit special tag with pointer inst
                const ptr_inst = extractDestroyPtrInst(datum, extra, tags, data) orelse {
                    // Can't determine ptr instruction, fall through to regular call
                    const call_parts = payloadCallParts(arena, ip, datum, extra, tags, data);
                    break :blk clr_allocator.allocPrint(arena, "    try Inst.call(state, {d}, {s}, {s});\n", .{ inst_index, call_parts.called, call_parts.args }, null);
                };
                const allocator_type = extractAllocatorType(ip, datum, extra, tags, data);
                break :blk clr_allocator.allocPrint(arena, "    try Inst.apply(state, {d}, .{{ .alloc_destroy = .{{ .ptr = {d}, .allocator_type = \"{s}\" }} }});\n", .{ inst_index, ptr_inst, allocator_type }, null);
            }
            const call_parts = payloadCallParts(arena, ip, datum, extra, tags, data);
            break :blk clr_allocator.allocPrint(arena, "    try Inst.call(state, {d}, {s}, {s});\n", .{ inst_index, call_parts.called, call_parts.args }, null);
        },
        else => blk: {
            const tag_payload = payload(arena, ip, tag, datum, inst_index, extra, param_names, arg_counter);
            break :blk clr_allocator.allocPrint(arena, "    try Inst.apply(state, {d}, .{{ .{s} = {s} }});\n", .{ inst_index, safeName(tag), tag_payload }, null);
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

/// Convert an AIR Ref to a Src union string.
/// Handles .eidx (runtime instruction), .interned (comptime), and .other (globals).
fn srcString(arena: std.mem.Allocator, ip: *const InternPool, ref: Ref) []const u8 {
    // Check for .none first (void/no value)
    if (ref == .none) {
        return ".{ .interned = .{ .void = {} } }";
    }
    if (ref.toIndex()) |idx| {
        // Runtime value from instruction
        return clr_allocator.allocPrint(arena, ".{{ .eidx = {d} }}", .{@intFromEnum(idx)}, null);
    }
    if (ref.toInterned()) |interned_idx| {
        // Check for null value first (untyped null)
        if (interned_idx == .null_value) {
            return ".{ .interned = .{ .null = {} } }";
        }
        // Comptime value - determine structure from type
        const ty = ip.typeOf(interned_idx);
        // Check for typed null only if type is NOT a well-known type (to avoid IP lookup in tests)
        if (!isWellKnownType(ty)) {
            const type_key = ip.indexToKey(ty);
            if (type_key == .opt_type) {
                // It's an optional type - check if the value is null
                const val_key = ip.indexToKey(interned_idx);
                if (val_key == .opt and val_key.opt.val == .none) {
                    // Include the child type so we know the structure even for null
                    const child_str = typeToString(arena, ip, type_key.opt_type);
                    return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .null = &{s} }} }}", .{child_str}, null);
                }
            }
        }
        const type_str = typeToString(arena, ip, ty);
        return clr_allocator.allocPrint(arena, ".{{ .interned = {s} }}", .{type_str}, null);
    }
    // Other (globals, etc.)
    return ".{ .other = {} }";
}

/// Check if a type index is a well-known type (doesn't require InternPool lookup).
fn isWellKnownType(ty: InternPool.Index) bool {
    return switch (ty) {
        .void_type, .noreturn_type, .none => true,
        .u8_type, .u16_type, .u32_type, .u64_type, .u128_type, .usize_type => true,
        .i8_type, .i16_type, .i32_type, .i64_type, .i128_type, .isize_type => true,
        .f16_type, .f32_type, .f64_type, .f80_type, .f128_type => true,
        .bool_type, .c_char_type, .comptime_int_type, .comptime_float_type => true,
        .undefined_type, .null_type, .anyerror_type, .type_type => true,
        .manyptr_u8_type, .manyptr_const_u8_type, .manyptr_const_u8_sentinel_0_type => true,
        .slice_const_u8_type, .slice_const_u8_sentinel_0_type => true,
        else => false,
    };
}

/// Convert an InternPool type to a Type union string.
/// Recursively handles nested types like pointers and optionals.
fn typeToString(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index) []const u8 {
    // Handle well-known type indices first (no InternPool lookup needed)
    return switch (ty) {
        .void_type => ".{ .void = {} }",
        .noreturn_type => ".{ .void = {} }",
        .none => ".{ .scalar = {} }",
        // Common scalar types
        .u8_type, .u16_type, .u32_type, .u64_type, .u128_type, .usize_type => ".{ .scalar = {} }",
        .i8_type, .i16_type, .i32_type, .i64_type, .i128_type, .isize_type => ".{ .scalar = {} }",
        .f16_type, .f32_type, .f64_type, .f80_type, .f128_type => ".{ .scalar = {} }",
        .bool_type, .c_char_type, .comptime_int_type, .comptime_float_type => ".{ .scalar = {} }",
        .undefined_type, .null_type, .anyerror_type, .type_type => ".{ .scalar = {} }",
        // Pointer types (well-known)
        .manyptr_u8_type, .manyptr_const_u8_type, .manyptr_const_u8_sentinel_0_type => ".{ .pointer = &.{ .scalar = {} } }",
        .slice_const_u8_type, .slice_const_u8_sentinel_0_type => ".{ .pointer = &.{ .scalar = {} } }",
        // For other types, need to look up in InternPool
        else => typeToStringLookup(arena, ip, ty),
    };
}

/// Look up a non-well-known type in the InternPool.
fn typeToStringLookup(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index) []const u8 {
    const type_key = ip.indexToKey(ty);
    return switch (type_key) {
        .simple_type => |simple| switch (simple) {
            .void => ".{ .void = {} }",
            .noreturn => ".{ .void = {} }",
            else => ".{ .scalar = {} }",
        },
        .ptr_type => |ptr| {
            const child_str = typeToString(arena, ip, ptr.child);
            return clr_allocator.allocPrint(arena, ".{{ .pointer = &{s} }}", .{child_str}, null);
        },
        .opt_type => |child| {
            // opt_type payload is the child type directly (not a struct with .child)
            const child_str = typeToString(arena, ip, child);
            return clr_allocator.allocPrint(arena, ".{{ .optional = &{s} }}", .{child_str}, null);
        },
        .struct_type => ".{ .@\"struct\" = {} }",
        .union_type => ".{ .@\"union\" = {} }",
        // All other types treated as scalar (int, float, array, enum, etc.)
        else => ".{ .scalar = {} }",
    };
}

fn payloadStore(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    // bin_op: lhs = destination ptr, rhs = source value
    const is_undef = isUndefRef(ip, datum.bin_op.rhs);
    const ptr: ?usize = if (datum.bin_op.lhs.toIndex()) |idx| @intFromEnum(idx) else null;
    const src_str = srcString(arena, ip, datum.bin_op.rhs);
    return clr_allocator.allocPrint(arena, ".{{ .ptr = {?d}, .src = {s}, .is_undef = {} }}", .{ ptr, src_str, is_undef }, null);
}

fn payloadLoad(arena: std.mem.Allocator, datum: Data) []const u8 {
    // Check for .none first (toIndex asserts on .none)
    if (datum.ty_op.operand == .none) {
        return ".{ .ptr = null }";
    }
    // TODO: handle global/interned pointers - for now emit null
    if (datum.ty_op.operand.toIndex()) |idx| {
        const ptr = @intFromEnum(idx);
        return clr_allocator.allocPrint(arena, ".{{ .ptr = {d} }}", .{ptr}, null);
    } else {
        return ".{ .ptr = null }";
    }
}

fn payloadDbg(arena: std.mem.Allocator, datum: Data, extra: []const u32) []const u8 {
    const operand = datum.pl_op.operand;
    const name_index = datum.pl_op.payload;

    // Extract the pointer instruction being named
    const ptr: ?usize = if (operand.toIndex()) |idx| @intFromEnum(idx) else null;

    // Extract the variable name from extra as NullTerminatedString
    const name = extractString(extra, name_index);

    return clr_allocator.allocPrint(arena, ".{{ .ptr = {?d}, .name = \"{s}\" }}", .{ ptr, name }, null);
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

    // Build args tuple string: .{ state.results[arg0].refinement.?, ... }
    var args_str: []const u8 = ".{";
    var first = true;

    var i: u32 = 0;
    while (i < args_len) : (i += 1) {
        const arg_ref: Ref = @enumFromInt(extra[payload_index + 1 + i]);
        if (arg_ref.toIndex()) |idx| {
            // Runtime value from local instruction - pass entity index
            const inst_idx = @intFromEnum(idx);
            if (first) {
                args_str = clr_allocator.allocPrint(arena, "{s} state.results[{d}].refinement.?", .{ args_str, inst_idx }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(arena, "{s}, state.results[{d}].refinement.?", .{ args_str, inst_idx }, null);
            }
        } else if (arg_ref.toInterned()) |interned_idx| {
            // Skip zero-sized types - they have no runtime representation
            const val_type = ip.typeOf(interned_idx);
            if (isZeroSizedType(ip, val_type)) continue;
            // Interned constant - create a defined scalar entity for it
            if (first) {
                args_str = clr_allocator.allocPrint(arena, "{s} try state.refinements.appendEntity(.{{ .scalar = .{{}} }})", .{args_str}, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(arena, "{s}, try state.refinements.appendEntity(.{{ .scalar = .{{}} }})", .{args_str}, null);
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

/// Generate parameter list string like "arg0: EIdx, arg1: EIdx"
fn buildParamList(arena: std.mem.Allocator, arg_count: u32) []const u8 {
    if (arg_count == 0) return "";

    var result: []const u8 = clr_allocator.allocPrint(arena, "arg0: EIdx", .{}, null);
    var i: u32 = 1;
    while (i < arg_count) : (i += 1) {
        result = clr_allocator.allocPrint(arena, "{s}, arg{d}: EIdx", .{ result, i }, null);
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

/// Extract allocator type from the first argument to create/destroy
/// Returns FQN like "PageAllocator" for comptime allocators, or traces inst_ref for runtime
fn extractAllocatorType(ip: *const InternPool, datum: Data, extra: []const u32, tags: []const Tag, data: []const Data) []const u8 {
    const payload_index = datum.pl_op.payload;
    const args_len = extra[payload_index];
    if (args_len == 0) return "unknown";

    // First argument is the allocator (self)
    const arg_ref: Ref = @enumFromInt(extra[payload_index + 1]);

    // Check if it's an interned value (comptime allocator)
    if (arg_ref.toInterned()) |ip_idx| {
        const arg_key = ip.indexToKey(ip_idx);
        switch (arg_key) {
            .aggregate => |agg| {
                // Allocator struct: [ptr, vtable]
                switch (agg.storage) {
                    .elems => |elems| {
                        if (elems.len >= 2) {
                            const vtable_elem = elems[1];
                            if (vtable_elem != .none) {
                                const vtable_key = ip.indexToKey(vtable_elem);
                                switch (vtable_key) {
                                    .ptr => |ptr| {
                                        switch (ptr.base_addr) {
                                            .nav => |nav_idx| {
                                                const nav = ip.getNav(nav_idx);
                                                const fqn = nav.fqn.toSlice(ip);
                                                // FQN is like "heap.PageAllocator.vtable"
                                                // Extract "PageAllocator"
                                                return extractAllocatorName(fqn);
                                            },
                                            else => {},
                                        }
                                    },
                                    else => {},
                                }
                            }
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
        return "comptime_unknown";
    }

    // It's an inst_ref - trace to find source
    if (arg_ref.toIndex()) |inst_idx| {
        return traceAllocatorType(tags, data, ip, @intFromEnum(inst_idx), 0);
    }

    return "unknown";
}

/// Extract allocator name from vtable FQN (e.g., "heap.PageAllocator.vtable" -> "PageAllocator")
fn extractAllocatorName(fqn: []const u8) []const u8 {
    // Find "heap." prefix and ".vtable" suffix
    const heap_prefix = "heap.";
    const vtable_suffix = ".vtable";

    var start: usize = 0;
    if (std.mem.indexOf(u8, fqn, heap_prefix)) |idx| {
        start = idx + heap_prefix.len;
    }

    var end = fqn.len;
    if (std.mem.indexOf(u8, fqn, vtable_suffix)) |idx| {
        end = idx;
    }

    if (start < end) {
        return fqn[start..end];
    }
    return fqn;
}

/// Trace through AIR to find allocator type from inst_ref
fn traceAllocatorType(tags: []const Tag, data: []const Data, ip: *const InternPool, inst_idx: usize, depth: u32) []const u8 {
    if (depth > 10) return "runtime_deep";
    if (inst_idx >= tags.len) return "runtime_oob";

    const tag = tags[inst_idx];
    const datum = data[inst_idx];

    switch (tag) {
        .load => {
            const ptr_ref = datum.ty_op.operand;
            if (ptr_ref.toIndex()) |src_idx| {
                return traceAllocatorType(tags, data, ip, @intFromEnum(src_idx), depth + 1);
            }
        },
        .call, .call_always_tail, .call_never_tail, .call_never_inline => {
            // Check if this is a call to .allocator() method
            const callee_ref = datum.pl_op.operand;
            if (callee_ref.toInterned()) |ip_idx| {
                const key = ip.indexToKey(ip_idx);
                switch (key) {
                    .func => |func_key| {
                        const nav = ip.getNav(func_key.owner_nav);
                        const fqn = nav.fqn.toSlice(ip);
                        // Check for pattern like "GeneralPurposeAllocator(...).allocator"
                        if (std.mem.indexOf(u8, fqn, ".allocator")) |_| {
                            // Extract the type name before .allocator
                            return extractTypeFromAllocatorMethod(fqn);
                        }
                    },
                    else => {},
                }
            }
        },
        .struct_field_ptr_index_0, .struct_field_ptr_index_1 => {
            // Trace through struct field access
            const base_ref = datum.ty_op.operand;
            if (base_ref.toIndex()) |src_idx| {
                return traceAllocatorType(tags, data, ip, @intFromEnum(src_idx), depth + 1);
            }
        },
        .bitcast => {
            // Trace through bitcast
            const src_ref = datum.ty_op.operand;
            if (src_ref.toIndex()) |src_idx| {
                return traceAllocatorType(tags, data, ip, @intFromEnum(src_idx), depth + 1);
            }
        },
        .alloc => {
            // We've reached a stack allocation - for runtime allocators, just mark as "Allocator"
            // TODO: Track allocator type through the typing system for better detection
            return "Allocator";
        },
        else => {},
    }

    return "runtime";
}

/// Extract allocator name from type FQN (e.g., "heap.GeneralPurposeAllocator(...)" -> "GeneralPurposeAllocator")
/// For "mem.Allocator" wrapper type, returns "Allocator" to indicate runtime allocator
fn extractAllocatorNameFromType(fqn: []const u8) []const u8 {
    // Check for mem.Allocator - this is a runtime wrapper type
    if (std.mem.eql(u8, fqn, "mem.Allocator")) {
        return "Allocator"; // Generic runtime allocator
    }

    const heap_prefix = "heap.";
    var start: usize = 0;
    if (std.mem.indexOf(u8, fqn, heap_prefix)) |idx| {
        start = idx + heap_prefix.len;
    }

    // Find the end - either "(" for generics or end of string
    var end = fqn.len;
    if (std.mem.indexOf(u8, fqn[start..], "(")) |idx| {
        end = start + idx;
    }

    if (start < end) {
        return fqn[start..end];
    }
    return fqn;
}

/// Extract type from allocator method FQN (e.g., "heap.GeneralPurposeAllocator(...).allocator" -> "GeneralPurposeAllocator")
fn extractTypeFromAllocatorMethod(fqn: []const u8) []const u8 {
    // Find the type name between "heap." and "(" or ".allocator"
    const heap_prefix = "heap.";
    var start: usize = 0;
    if (std.mem.indexOf(u8, fqn, heap_prefix)) |idx| {
        start = idx + heap_prefix.len;
    }

    // Find the end - either "(" for generics or ".allocator"
    var end = fqn.len;
    if (std.mem.indexOf(u8, fqn[start..], "(")) |idx| {
        end = start + idx;
    } else if (std.mem.indexOf(u8, fqn[start..], ".allocator")) |idx| {
        end = start + idx;
    }

    if (start < end) {
        return fqn[start..end];
    }
    return fqn;
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
fn extractDestroyPtrInst(datum: Data, extra: []const u32, tags: []const Tag, data: []const Data) ?usize {
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

// =============================================================================
// FunctionGen - Represents a function to generate (main or branch)
// =============================================================================

const SubTag = enum { cond_br_true, cond_br_false };

const FunctionGen = union(enum) {
    /// Full function (entry point into the codegen)
    full: struct {
        func_index: u32,
    },
    /// Sub function (branch of a conditional)
    sub: struct {
        func_index: u32,
        instr_index: u32,
        tag: SubTag,
        body_indices: []const u32,
    },

    /// Generate function name
    fn name(self: FunctionGen, arena: std.mem.Allocator) []const u8 {
        return switch (self) {
            .full => |f| clr_allocator.allocPrint(arena, "fn_{d}", .{f.func_index}, null),
            .sub => |s| clr_allocator.allocPrint(arena, "fn_{d}_{s}_{d}", .{
                s.func_index,
                @tagName(s.tag),
                s.instr_index,
            }, null),
        };
    }

    /// Generate function signature/header (opening)
    /// For sub functions, discard_caller_params is unused (kept for API compatibility)
    fn header(self: FunctionGen, arena: std.mem.Allocator, params: []const u8, fqn: []const u8, file_path: []const u8, base_line: u32, num_insts: usize, discard_caller_params: bool) []const u8 {
        _ = discard_caller_params;
        return switch (self) {
            .full => |f| clr_allocator.allocPrint(arena,
                \\fn fn_{d}(ctx: *Context, caller_refinements: ?*Refinements{s}) anyerror!EIdx {{
                \\    ctx.meta.file = "{s}";
                \\    ctx.base_line = {d};
                \\    try ctx.push_fn("{s}");
                \\    defer ctx.pop_fn();
                \\
                \\    var refinements = Refinements.init(ctx.allocator);
                \\    defer refinements.deinit();
                \\
                \\    const results = try Inst.make_results_list(ctx.allocator, {d});
                \\    defer Inst.clear_results_list(results, ctx.allocator);
                \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{{ .retval_future = {{}} }}) else 0;
                \\
                \\    const state = State{{ .ctx = ctx, .results = results, .refinements = &refinements, .return_eidx = return_eidx, .caller_refinements = caller_refinements }};
                \\
                \\
            , .{ f.func_index, params, file_path, base_line, fqn, num_insts }, null),
            .sub => |s| clr_allocator.allocPrint(arena,
                \\fn fn_{d}_{s}_{d}(state: State) anyerror!void {{
                \\
            , .{ s.func_index, @tagName(s.tag), s.instr_index }, null),
        };
    }

    /// Generate function footer (closing)
    fn footer(self: FunctionGen, arena: std.mem.Allocator) []const u8 {
        return switch (self) {
            .full => clr_allocator.allocPrint(arena,
                \\    try Inst.onFinish(state);
                \\    Inst.backPropagate(state);
                \\    return return_eidx;
                \\}}
                \\
            , .{}, null),
            .sub => clr_allocator.allocPrint(arena,
                \\}}
                \\
                \\
            , .{}, null),
        };
    }

    /// Check if this is a sub function (needs different refinements handling)
    fn isSub(self: FunctionGen) bool {
        return self == .sub;
    }

};

/// Extract body indices from a block instruction
/// Returns null if extraction fails
fn extractBlockBody(block_idx: u32, data: []const Data, extra: []const u32) ?[]const u32 {
    if (block_idx >= data.len) return null;
    const block_data = data[block_idx];
    // Raw access: ty_pl has ty (u32) then payload (u32), so payload is at byte offset 4
    const raw_ptr: [*]const u8 = @ptrCast(&block_data);
    const payload_ptr: *const u32 = @ptrCast(@alignCast(raw_ptr + 4));
    const payload_index = payload_ptr.*;
    if (payload_index >= extra.len) return null;
    const body_len = extra[payload_index];
    if (body_len == 0) return null;

    const body_indices_start = payload_index + 1;
    if (body_indices_start + body_len > extra.len) return null;
    return extra[body_indices_start..][0..body_len];
}

/// Extract then and else body indices from a cond_br instruction
/// Returns null if extraction fails
fn extractCondBrBodies(cond_br_idx: usize, data: []const Data, extra: []const u32) ?struct { then: []const u32, @"else": []const u32 } {
    if (cond_br_idx >= data.len) return null;
    const cond_br_datum = data[cond_br_idx];
    const cond_br_payload = cond_br_datum.pl_op.payload;
    if (cond_br_payload + 3 > extra.len) return null;

    const then_body_len = extra[cond_br_payload];
    const else_body_len = extra[cond_br_payload + 1];
    // branch_hints at extra[cond_br_payload + 2] - we don't use them

    const then_body_start = cond_br_payload + 3;
    const else_body_start = then_body_start + then_body_len;
    if (else_body_start + else_body_len > extra.len) return null;

    return .{
        .then = extra[then_body_start..][0..then_body_len],
        .@"else" = extra[else_body_start..][0..else_body_len],
    };
}

/// Generate Zig source code for a function from AIR instructions using worklist algorithm.
/// This processes the main function and any nested conditionals, outputting branch functions
/// first (so they're in scope) followed by the main function.
pub fn generateFunction(func_index: u32, fqn: []const u8, ip: *const InternPool, tags: []const Tag, data: []const Data, extra: []const u32, base_line: u32, file_path: []const u8, param_names: []const []const u8) []u8 {
    if (tags.len == 0) @panic("function with no instructions encountered");

    // Per-function arena for temporary allocations
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Count args and build parameter list (only for full functions)
    const arg_count = countArgs(tags);
    const param_list = buildParamList(arena.allocator(), arg_count);
    const params: []const u8 = if (arg_count > 0)
        clr_allocator.allocPrint(arena.allocator(), ", {s}", .{param_list}, null)
    else
        "";

    // Worklist of functions to generate
    var worklist: std.ArrayListUnmanaged(FunctionGen) = .empty;
    // Output: generated functions (sub functions first, main last)
    var sub_functions: std.ArrayListUnmanaged([]const u8) = .empty;
    var sub_functions_len: usize = 0;

    // Start with the main function
    worklist.append(arena.allocator(), .{ .full = .{ .func_index = func_index } }) catch @panic("out of memory");

    var main_function: []const u8 = "";

    while (worklist.items.len > 0) {
        const item = worklist.pop().?;
        // Generate this function
        const func_str = generateOneFunction(
            arena.allocator(),
            item,
            func_index,
            fqn,
            ip,
            tags,
            data,
            extra,
            params,
            file_path,
            base_line,
            param_names,
            &worklist,
        );

        switch (item) {
            .full => {
                main_function = func_str;
            },
            .sub => {
                sub_functions.append(arena.allocator(), func_str) catch @panic("out of memory");
                sub_functions_len += func_str.len;
            },
        }
    }

    // Combine: sub functions first, then main function
    if (sub_functions_len == 0) {
        // No sub functions - just copy main to persistent allocator
        const result = clr_allocator.allocator().alloc(u8, main_function.len) catch @panic("out of memory");
        @memcpy(result, main_function);
        return result;
    }

    const total_len = sub_functions_len + main_function.len;
    const result = clr_allocator.allocator().alloc(u8, total_len) catch @panic("out of memory");
    var pos: usize = 0;

    // Sub functions first
    for (sub_functions.items) |func| {
        @memcpy(result[pos..][0..func.len], func);
        pos += func.len;
    }

    // Main function last
    @memcpy(result[pos..][0..main_function.len], main_function);

    return result;
}

/// Generate a single function (full or sub) using backwards walk + stack approach
fn generateOneFunction(
    arena: std.mem.Allocator,
    item: FunctionGen,
    func_index: u32,
    fqn: []const u8,
    ip: *const InternPool,
    tags: []const Tag,
    data: []const Data,
    extra: []const u32,
    params: []const u8,
    file_path: []const u8,
    base_line: u32,
    param_names: []const []const u8,
    worklist: *std.ArrayListUnmanaged(FunctionGen),
) []const u8 {
    // For full functions, collect cond_br branch body indices to skip
    // (they're processed in sub functions, not main)
    var skip_set: std.AutoHashMapUnmanaged(u32, void) = .empty;
    if (item == .full) {
        // Find all cond_br instructions and mark their then/else body indices for skipping
        for (tags, 0..) |tag, idx| {
            if (tag != .cond_br) continue;
            if (extractCondBrBodies(idx, data, extra)) |bodies| {
                for (bodies.then) |body_idx| {
                    skip_set.put(arena, body_idx, {}) catch @panic("out of memory");
                }
                for (bodies.@"else") |body_idx| {
                    skip_set.put(arena, body_idx, {}) catch @panic("out of memory");
                }
            }
        }
    }

    // Determine which indices to process
    const indices: []const u32 = switch (item) {
        .full => blk: {
            // Full function: all indices 0..tags.len
            const all_indices = arena.alloc(u32, tags.len) catch @panic("out of memory");
            for (all_indices, 0..) |*slot, i| {
                slot.* = @intCast(i);
            }
            break :blk all_indices;
        },
        .sub => |s| s.body_indices,
    };

    // Stack for instruction data (will be reversed)
    const InstrData = struct {
        idx: u32,
        tag: Tag,
        datum: Data,
    };
    var stack: std.ArrayListUnmanaged(InstrData) = .empty;

    // Walk backwards through indices, pushing to stack
    // When we hit cond_br, add its branches to worklist
    var i: usize = indices.len;
    while (i > 0) {
        i -= 1;
        const idx = indices[i];
        if (idx >= tags.len) continue;

        // For full functions, skip cond_br branch body indices (they're in sub functions)
        if (item == .full and skip_set.contains(idx)) {
            continue;
        }

        const tag = tags[idx];
        const datum = data[idx];

        // Check for cond_br - add branches to worklist
        if (tag == .cond_br) {
            if (extractCondBrBodies(idx, data, extra)) |bodies| {
                // Add both branches to worklist
                worklist.append(arena, .{ .sub = .{
                    .func_index = func_index,
                    .instr_index = idx,
                    .tag = .cond_br_true,
                    .body_indices = bodies.then,
                } }) catch @panic("out of memory");
                worklist.append(arena, .{ .sub = .{
                    .func_index = func_index,
                    .instr_index = idx,
                    .tag = .cond_br_false,
                    .body_indices = bodies.@"else",
                } }) catch @panic("out of memory");
            }
        }

        // Push all instructions to stack (they'll be processed in forward order when popped)
        stack.append(arena, .{ .idx = idx, .tag = tag, .datum = datum }) catch @panic("out of memory");
    }

    // Pop from stack to generate lines in forward order
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;
    var arg_counter: u32 = 0;

    while (stack.items.len > 0) {
        const instr = stack.pop().?;
        var line: []const u8 = undefined;

        if (instr.tag == .cond_br) {
            // Generate Inst.cond_br call with function references
            line = generateCondBrLine(arena, instr.idx, func_index);
        } else {
            line = _instLine(arena, ip, instr.tag, instr.datum, instr.idx, extra, tags, data, param_names, &arg_counter);
        }

        lines.append(arena, line) catch @panic("out of memory");
        total_len += line.len;
    }

    // Concatenate all lines
    const body_lines = arena.alloc(u8, total_len) catch @panic("out of memory");
    var pos: usize = 0;
    for (lines.items) |line| {
        @memcpy(body_lines[pos..][0..line.len], line);
        pos += line.len;
    }

    // For sub functions, check if body contains ret_safe - if so, don't discard caller params
    const discard_caller_params = switch (item) {
        .full => false, // doesn't matter for full functions
        .sub => |s| blk: {
            for (s.body_indices) |body_idx| {
                if (tags[body_idx] == .ret_safe) break :blk false;
            }
            break :blk true;
        },
    };

    // Generate complete function with header/footer
    const header_str = item.header(arena, params, fqn, file_path, base_line, tags.len, discard_caller_params);
    const footer_str = item.footer(arena);

    const func_len = header_str.len + body_lines.len + footer_str.len;
    const func_buf = arena.alloc(u8, func_len) catch @panic("out of memory");
    pos = 0;
    @memcpy(func_buf[pos..][0..header_str.len], header_str);
    pos += header_str.len;
    @memcpy(func_buf[pos..][0..body_lines.len], body_lines);
    pos += body_lines.len;
    @memcpy(func_buf[pos..][0..footer_str.len], footer_str);

    return func_buf;
}

/// Generate the Inst.cond_br call line with new naming scheme
fn generateCondBrLine(arena: std.mem.Allocator, cond_br_idx: u32, func_index: u32) []const u8 {
    return clr_allocator.allocPrint(arena,
        \\    try Inst.cond_br(state, {d}, fn_{d}_cond_br_true_{d}, fn_{d}_cond_br_false_{d});
        \\
    , .{ cond_br_idx, func_index, cond_br_idx, func_index, cond_br_idx }, null);
}

/// Generate epilogue with imports and main function
pub fn epilogue(entrypoint_index: u32) []u8 {
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Inst = clr.Inst;
        \\const Refinements = clr.Refinements;
        \\const EIdx = clr.EIdx;
        \\const State = clr.State;
        \\
        \\var writer_buf: [4096]u8 = undefined;
        \\var file_writer: std.fs.File.Writer = undefined;
        \\
        \\pub fn main() void {{
        \\    var gpa = std.heap.GeneralPurposeAllocator(.{{}}){{}};
        \\    defer _ = gpa.deinit();
        \\    const allocator = gpa.allocator();
        \\    file_writer = std.fs.File.stdout().writer(&writer_buf);
        \\    defer file_writer.interface.flush() catch {{}};
        \\    var ctx = Context.init(allocator, &file_writer.interface);
        \\    defer ctx.deinit();
        \\    _ = fn_{d}(&ctx, null) catch {{
        \\        file_writer.interface.flush() catch {{}};
        \\        std.process.exit(1);
        \\    }};
        \\}}
        \\
    , .{entrypoint_index}, null);
}

/// Generate a stub function for a missing call target
pub fn generateStub(func_index: u32, arity: u32) []u8 {
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Build parameter list: ctx + caller_refinements + arity EIdx args
    var params: []const u8 = "ctx: *Context, caller_refinements: ?*Refinements";
    var i: u32 = 0;
    while (i < arity) : (i += 1) {
        params = clr_allocator.allocPrint(arena.allocator(), "{s}, _: EIdx", .{params}, null);
    }

    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\fn fn_{d}({s}) anyerror!EIdx {{
        \\    std.debug.print("WARNING: call to unresolved function fn_{d}\\n", .{{}});
        \\    ctx.dumpStackTrace();
        \\    return if (caller_refinements) |cp| try cp.appendEntity(.{{ .retval_future = {{}} }}) else 0;
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
                // Skip allocator create/destroy calls - they are transformed, not called
                if (isAllocatorCreate(ip, datum)) continue;
                if (isAllocatorDestroy(ip, datum)) continue;

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
