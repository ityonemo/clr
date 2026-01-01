const clr_allocator = @import("allocator.zig");
const clr = @import("clr.zig");
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

/// Re-export types from clr namespace for external use
pub const NameMap = clr.NameMap;
pub const FnInfo = clr.FnInfo;

/// Register a name with a specific ID (typically an InternPool index).
/// Use this when you have an InternPool NullTerminatedString to ensure globally unique IDs.
fn registerNameWithId(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), id: u32, name: []const u8) void {
    const allocator = clr_allocator.allocator();

    // Check if already in map with this ID
    if (name_map.contains(id)) return;

    const duped = allocator.dupe(u8, name) catch @panic("OOM");
    name_map.put(allocator, id, duped) catch @panic("OOM");
}

/// Register a name using a hash-based ID. Use this for names without InternPool indices
/// (e.g., ZIR param names, constructed strings). Hash ensures same name = same ID across functions.
fn registerName(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), name: []const u8) u32 {
    const allocator = clr_allocator.allocator();

    // Use hash of name as ID (ensures same name = same ID globally)
    // Use upper bit to avoid collision with InternPool indices (which are typically smaller)
    const hash = std.hash.Wyhash.hash(0, name);
    const id: u32 = @truncate(hash | 0x80000000); // Set high bit to distinguish from IP indices

    // Check if already in map
    if (name_map.contains(id)) return id;

    const duped = allocator.dupe(u8, name) catch @panic("OOM");
    name_map.put(allocator, id, duped) catch @panic("OOM");
    return id;
}

/// Register a field name mapping: {type_id, field_index} -> name_id
/// Only registers if name_id is non-zero (field has a name).
fn registerFieldName(field_map: *clr.FieldHashMap, type_id: u32, field_index: u32, name_id: u32) void {
    if (name_id == 0) return; // No name to register
    const allocator = clr_allocator.allocator();
    const key = clr.FieldKey{ .type_id = type_id, .field_index = field_index };
    field_map.put(allocator, key, name_id) catch @panic("OOM");
}

/// Returns alternative names, used for:
/// - a keyword-safe tag name using @"..." syntax to handle reserved keywords 
/// - naming overloads
fn altName(tag: Tag) []const u8 {
    return switch (tag) {
        .@"try" => "@\"try\"",
        .struct_field_ptr_index_0, .struct_field_ptr_index_1, .struct_field_ptr_index_2, .struct_field_ptr_index_3 => "struct_field_ptr",
        else => @tagName(tag),
    };
}

/// Returns the payload string for a given tag and data.
/// Note: call tags are handled separately in buildInstLines via payloadCallParts.
fn payload(info: *const FnInfo, tag: Tag, datum: Data, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .alloc => payloadAlloc(info, datum),
        .arg => payloadArg(info, datum, arg_counter),
        .dbg_stmt => payloadDbgStmt(info, datum),
        .store, .store_safe => payloadStore(info, datum),
        .load => payloadLoad(info, datum),
        .ret_safe => payloadRetSafe(info, datum),
        .dbg_var_ptr, .dbg_var_val, .dbg_arg_inline => payloadDbg(info, datum),
        .bitcast => payloadBitcast(info, datum),
        .unwrap_errunion_payload, .optional_payload => payloadTransferOp(info, datum),
        .is_non_null, .is_null => payloadUnOp(info, datum),
        .br => payloadBr(info, datum),
        .block => payloadBlock(info, datum),
        .struct_field_ptr_index_0 => payloadStructFieldPtr(info, datum, 0),
        .struct_field_ptr_index_1 => payloadStructFieldPtr(info, datum, 1),
        .struct_field_ptr_index_2 => payloadStructFieldPtr(info, datum, 2),
        .struct_field_ptr_index_3 => payloadStructFieldPtr(info, datum, 3),
        // Note: All struct_field_ptr_index_N are mapped to struct_field_ptr via altName()
        .struct_field_val => payloadStructFieldVal(info, datum),
        .field_parent_ptr => payloadFieldParentPtr(info, datum),
        .ret_ptr => payloadRetPtr(info, datum),
        .ret_load => payloadRetLoad(info, datum),
        .set_union_tag => payloadSetUnionTag(info, datum),
        .get_union_tag => payloadGetUnionTag(info, datum),
        .@"try", .try_cold => payloadTry(info, datum),
        else => ".{}",
    };
}

/// Payload for operations that transfer all properties from source to result.
/// Used for unwrap_errunion_payload, optional_payload, etc. where the result carries
/// the same memory safety / allocation state as the source.
fn payloadTransferOp(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info.arena, info.ip, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for .try/.try_cold - extracts success payload from error union.
/// Uses pl_op.operand to get the error union source.
fn payloadTry(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.pl_op.operand;
    const src_str = srcString(info.arena, info.ip, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for bitcast - includes destination type for analysis.
fn payloadBitcast(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info.arena, info.ip, operand);
    const ty_str = if (datum.ty_op.ty.toInternedAllowNone()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .id = null, .ty = .{ .scalar = {} } }";
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s}, .ty = {s} }}", .{ src_str, ty_str }, null);
}

/// Payload for un_op instructions (is_non_null, is_null, etc.) that use datum.un_op
fn payloadUnOp(info: *const FnInfo, datum: Data) []const u8 {
    const src_str = srcString(info.arena, info.ip, datum.un_op);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for ret_safe - just the src, caller_refinements and return_eidx come from State.
fn payloadRetSafe(info: *const FnInfo, datum: Data) []const u8 {
    const src_str = srcString(info.arena, info.ip, datum.un_op);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for alloc - extracts pointee type from the pointer type.
fn payloadAlloc(info: *const FnInfo, datum: Data) []const u8 {
    const ptr_type = datum.ty.toIntern();
    // Extract pointee type from pointer type
    const pointee_type: InternPool.Index = switch (ptr_type) {
        // Well-known pointer types - extract pointee directly
        .manyptr_u8_type, .manyptr_const_u8_type => .u8_type,
        .manyptr_const_u8_sentinel_0_type => .u8_type,
        .slice_const_u8_type, .slice_const_u8_sentinel_0_type => .u8_type,
        else => blk: {
            // For other types, look up in InternPool
            const ptr_key = info.ip.indexToKey(ptr_type);
            break :blk switch (ptr_key) {
                .ptr_type => |p| p.child,
                else => .none, // fallback
            };
        },
    };
    const pointee_str = typeToString(info.name_map, info.field_map, info.arena, info.ip, pointee_type);
    return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s} }}", .{pointee_str}, null);
}

/// Extract type info for AllocCreate payload.
/// allocator.create(T) returns Allocator.Error!*T, so we unwrap error union then pointer.
/// NOTE: Returns ".{ .unknown = {} }" when type cannot be determined, which will cause
/// a compile error in the generated .air.zig - this is intentional to surface extraction failures.
/// TODO: Audit other places in codegen that fall back to scalar and consider using .unknown instead.
fn extractAllocCreateType(arena: std.mem.Allocator, ip: *const InternPool, datum: Data) []const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return ".{ .unknown = {} }";

    // Get function and its type
    const func_key = ip.indexToKey(ip_idx);
    const func_ty = switch (func_key) {
        .func => |f| f.ty,
        else => return ".{ .unknown = {} }",
    };

    // Get function type to access return_type
    const func_type_key = ip.indexToKey(func_ty);
    const return_type = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return ".{ .unknown = {} }",
    };

    // Return type is Allocator.Error!*T - unwrap error union to get *T
    const ptr_type: InternPool.Index = blk: {
        const return_key = ip.indexToKey(return_type);
        break :blk switch (return_key) {
            .error_union_type => |eu| eu.payload_type,
            .ptr_type => return_type, // Already unwrapped
            else => return ".{ .unknown = {} }",
        };
    };

    // Now unwrap *T to get T
    const pointee_type: InternPool.Index = blk: {
        const ptr_key = ip.indexToKey(ptr_type);
        break :blk switch (ptr_key) {
            .ptr_type => |p| p.child,
            else => return ".{ .unknown = {} }",
        };
    };

    return typeToString(null, null, arena, ip, pointee_type);
}

/// Payload for struct_field_ptr_index_N - gets pointer to a struct field.
/// Field names are looked up at runtime via ctx.getFieldId(type_id, field_index).
fn payloadStructFieldPtr(info: *const FnInfo, datum: Data, field_index: usize) []const u8 {
    // ty_op.ty is the result type (pointer to field), ty_op.operand is the base struct pointer
    const ty_str = if (datum.ty_op.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unknown = {} }"; // Will cause compile error if hit
    const base_ptr: ?usize = if (datum.ty_op.operand.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(info.arena, ".{{ .base = {?d}, .field_index = {d}, .ty = {s} }}", .{ base_ptr, field_index, ty_str }, null);
}

/// Payload for struct_field_val - extracts a field value from a struct by value.
/// Uses ty_pl with StructField payload: struct_operand and field_index.
/// Field names are looked up at runtime via ctx.getFieldId(type_id, field_index).
fn payloadStructFieldVal(info: *const FnInfo, datum: Data) []const u8 {
    // ty_pl has ty (Ref) and payload index (u32)
    const ty_str = if (datum.ty_pl.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unknown = {} }"; // Will cause compile error if hit
    const payload_index = datum.ty_pl.payload;

    // StructField: struct_operand (Ref as u32), field_index (u32)
    const struct_operand_raw = info.extra[payload_index];
    const field_index = info.extra[payload_index + 1];

    // Convert the raw u32 to a Ref and extract the index
    const struct_operand: Ref = @enumFromInt(struct_operand_raw);
    const operand: ?usize = if (struct_operand.toIndex()) |idx| @intFromEnum(idx) else null;

    return clr_allocator.allocPrint(info.arena, ".{{ .operand = {?d}, .field_index = {d}, .ty = {s} }}", .{ operand, field_index, ty_str }, null);
}

/// Payload for field_parent_ptr - gets parent container pointer from field pointer.
/// Uses ty_pl with FieldParentPtr payload: field_ptr and field_index.
fn payloadFieldParentPtr(info: *const FnInfo, datum: Data) []const u8 {
    // ty_pl has ty (result type - pointer to parent) and payload index
    const ty_str = if (datum.ty_pl.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unknown = {} }";
    const payload_index = datum.ty_pl.payload;

    // FieldParentPtr: field_ptr (Ref as u32), field_index (u32)
    const field_ptr_raw = info.extra[payload_index];
    const field_index = info.extra[payload_index + 1];

    // Convert the raw u32 to a Ref and extract the index
    const field_ptr_ref: Ref = @enumFromInt(field_ptr_raw);
    const field_ptr: ?usize = if (field_ptr_ref.toIndex()) |idx| @intFromEnum(idx) else null;

    return clr_allocator.allocPrint(info.arena, ".{{ .field_ptr = {?d}, .field_index = {d}, .ty = {s} }}", .{ field_ptr, field_index, ty_str }, null);
}

/// Payload for br (branch to block with value).
/// Transfers properties from operand to target block.
fn payloadBr(info: *const FnInfo, datum: Data) []const u8 {
    const block_inst = @intFromEnum(datum.br.block_inst);
    const operand = datum.br.operand;
    const src_str = srcString(info.arena, info.ip, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .block = {d}, .src = {s} }}", .{ block_inst, src_str }, null);
}

/// Payload for block - extracts the block's result type.
fn payloadBlock(info: *const FnInfo, datum: Data) []const u8 {
    const ty_ref = datum.ty_pl.ty;

    // Check for well-known types that don't require InternPool lookup
    if (ty_ref == .none) {
        return ".{ .ty = .{ .id = null, .ty = .{ .void = {} } } }";
    }
    if (ty_ref == .void_type) {
        return ".{ .ty = .{ .id = null, .ty = .{ .void = {} } } }";
    }
    if (ty_ref == .noreturn_type) {
        return ".{ .ty = .{ .id = null, .ty = .{ .void = {} } } }";
    }

    // Try to get interned index
    const ty_idx = ty_ref.toInternedAllowNone() orelse return ".{ .ty = .{ .id = null, .ty = .{ .void = {} } } }";

    // Use typeToString which handles well-known types first
    const ty_str = typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx);
    return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s} }}", .{ty_str}, null);
}

/// Generate a single inst line for a given tag and data.
/// Exposed for testing with underscore prefix to indicate internal use.
/// arg_counter tracks sequential arg indices (may differ from zir_param_index).
pub fn _instLine(info: *const FnInfo, tag: Tag, datum: Data, inst_index: usize, arg_counter: ?*u32) []const u8 {
    return switch (tag) {
        .call, .call_always_tail, .call_never_tail, .call_never_inline => blk: {
            if (isDebugCall(info.ip, datum)) {
                break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .noop_pruned_debug = .{{}} }});\n", .{inst_index}, null);
            }
            if (isAllocatorCreate(info.ip, datum)) {
                // Prune allocator.create() - emit special tag for tracking
                const allocator_info = extractAllocatorType(info.ip, datum, info.extra, info.tags, info.data);
                registerNameWithId(info.name_map, allocator_info.id, allocator_info.name);
                const created_type = extractAllocCreateType(info.arena, info.ip, datum);
                break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .alloc_create = .{{ .type_id = {d}, .ty = {s} }} }});\n", .{ inst_index, allocator_info.id, created_type }, null);
            }
            if (isAllocatorDestroy(info.ip, datum)) {
                // Prune allocator.destroy() - emit special tag with pointer inst
                const ptr_inst = extractDestroyPtrInst(datum, info.extra, info.tags, info.data) orelse {
                    // Can't determine ptr instruction, fall through to regular call
                    const call_parts = payloadCallParts(info, datum);
                    break :blk clr_allocator.allocPrint(info.arena, "    try Inst.call(state, {d}, {s}, {s});\n", .{ inst_index, call_parts.called, call_parts.args }, null);
                };
                const allocator_info = extractAllocatorType(info.ip, datum, info.extra, info.tags, info.data);
                registerNameWithId(info.name_map, allocator_info.id, allocator_info.name);
                break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .alloc_destroy = .{{ .ptr = {d}, .type_id = {d} }} }});\n", .{ inst_index, ptr_inst, allocator_info.id }, null);
            }
            const call_parts = payloadCallParts(info, datum);
            break :blk clr_allocator.allocPrint(info.arena, "    try Inst.call(state, {d}, {s}, {s});\n", .{ inst_index, call_parts.called, call_parts.args }, null);
        },
        else => blk: {
            const tag_payload = payload(info, tag, datum, arg_counter);
            break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .{s} = {s} }});\n", .{ inst_index, altName(tag), tag_payload }, null);
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
fn payloadArg(info: *const FnInfo, datum: Data, arg_counter: ?*u32) []const u8 {
    const zir_param_index = datum.arg.zir_param_index;
    const name = if (zir_param_index < info.param_names.len) info.param_names[zir_param_index] else "";
    const name_id = registerName(info.name_map, name);
    const arg_index = if (arg_counter) |counter| blk: {
        const idx = counter.*;
        counter.* += 1;
        break :blk idx;
    } else zir_param_index;
    return clr_allocator.allocPrint(info.arena, ".{{ .value = arg{d}, .name_id = {d} }}", .{ arg_index, name_id }, null);
}

fn payloadDbgStmt(info: *const FnInfo, datum: Data) []const u8 {
    return clr_allocator.allocPrint(info.arena, ".{{ .line = {d}, .column = {d} }}", .{
        datum.dbg_stmt.line,
        datum.dbg_stmt.column,
    }, null);
}

/// Convert an AIR Ref to a Src union string.
/// Handles .eidx (runtime instruction), .interned (comptime), and .other (globals).
fn srcString(arena: std.mem.Allocator, ip: *const InternPool, ref: Ref) []const u8 {
    // Check for .none first (void/no value)
    if (ref == .none) {
        return ".{ .interned = .{ .id = null, .ty = .{ .void = {} } } }";
    }
    if (ref.toIndex()) |idx| {
        // Runtime value from instruction
        return clr_allocator.allocPrint(arena, ".{{ .eidx = {d} }}", .{@intFromEnum(idx)}, null);
    }
    if (ref.toInterned()) |interned_idx| {
        // Check for null value first (untyped null)
        if (interned_idx == .null_value) {
            // Untyped null - use scalar as fallback since we don't know the optional type
            return ".{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } }";
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
                    const child_str = typeToString(null, null, arena, ip, type_key.opt_type);
                    return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .id = null, .ty = .{{ .null = &{s} }} }} }}", .{child_str}, null);
                }
            }
            // Check for aggregate value (struct) with field-level undefined
            if (type_key == .struct_type) {
                const val_key = ip.indexToKey(interned_idx);
                if (val_key == .aggregate) {
                    const agg = val_key.aggregate;
                    const struct_str = aggregateValueToString(arena, ip, ty, agg);
                    return clr_allocator.allocPrint(arena, ".{{ .interned = {s} }}", .{struct_str}, null);
                }
            }
        }
        const type_str = typeToString(null, null, arena, ip, ty);
        return clr_allocator.allocPrint(arena, ".{{ .interned = {s} }}", .{type_str}, null);
    }
    // Other (globals, etc.)
    return ".{ .other = {} }";
}

/// Convert an AIR Ref to a Src union string with .undefined wrapper.
/// Used when storing undefined values - wraps the type in .undefined.
fn srcStringUndef(arena: std.mem.Allocator, ip: *const InternPool, ref: Ref) []const u8 {
    // For undefined stores, we wrap the type in .undefined
    if (ref.toInterned()) |interned_idx| {
        const ty = ip.typeOf(interned_idx);
        const type_str = typeToString(null, null, arena, ip, ty);
        return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .id = null, .ty = .{{ .undefined = &{s} }} }} }}", .{type_str}, null);
    }
    // Fallback - shouldn't happen for undefined stores
    return ".{ .interned = .{ .id = null, .ty = .{ .undefined = &.{ .id = null, .ty = .{ .scalar = {} } } } } }";
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

/// Set of visited type indices for cycle detection in recursive types.
const VisitedTypes = std.AutoArrayHashMapUnmanaged(InternPool.Index, void);

/// Convert an InternPool type to a Type union string.
/// Recursively handles nested types like pointers and optionals.
/// Note: name_map is optional - pass null when type stringification doesn't need name registration.
fn typeToString(name_map: ?*std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index) []const u8 {
    var visited = VisitedTypes{};
    return typeToStringInner(name_map, field_map, arena, ip, ty, &visited);
}

/// Inner function with cycle detection via visited set.
fn typeToStringInner(name_map: ?*std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
    // Handle well-known type indices first (no InternPool lookup needed)
    return switch (ty) {
        .void_type => ".{ .id = null, .ty = .{ .void = {} } }",
        .noreturn_type => ".{ .id = null, .ty = .{ .void = {} } }",
        .none => ".{ .id = null, .ty = .{ .scalar = {} } }",
        // Common scalar types
        .u8_type, .u16_type, .u32_type, .u64_type, .u128_type, .usize_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        .i8_type, .i16_type, .i32_type, .i64_type, .i128_type, .isize_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        .f16_type, .f32_type, .f64_type, .f80_type, .f128_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        .bool_type, .c_char_type, .comptime_int_type, .comptime_float_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        .undefined_type, .null_type, .anyerror_type, .type_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        // Pointer types (well-known)
        .manyptr_u8_type, .manyptr_const_u8_type, .manyptr_const_u8_sentinel_0_type => ".{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } }",
        .slice_const_u8_type, .slice_const_u8_sentinel_0_type => ".{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } }",
        // For other types, need to look up in InternPool
        else => if (name_map) |nm| typeToStringLookup(nm, field_map, arena, ip, ty, visited) else typeToStringLookupNoNames(arena, ip, ty, visited),
    };
}

/// Look up a non-well-known type without name registration (for callers without name_map).
fn typeToStringLookupNoNames(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
    if (visited.contains(ty)) {
        return ".{ .id = null, .ty = .{ .scalar = {} } }";
    }
    visited.put(arena, ty, {}) catch @panic("out of memory");

    const type_key = ip.indexToKey(ty);
    return switch (type_key) {
        .simple_type => |simple| switch (simple) {
            .void => ".{ .id = null, .ty = .{ .void = {} } }",
            .noreturn => ".{ .id = null, .ty = .{ .void = {} } }",
            else => ".{ .id = null, .ty = .{ .scalar = {} } }",
        },
        .ptr_type => |ptr| blk: {
            const child_str = typeToStringInner(null, null, arena, ip, ptr.child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .pointer = &{s} }} }}", .{child_str}, null);
        },
        .opt_type => |child| blk: {
            const child_str = typeToStringInner(null, null, arena, ip, child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .optional = &{s} }} }}", .{child_str}, null);
        },
        .error_union_type => |eu| blk: {
            const payload_str = typeToStringInner(null, null, arena, ip, eu.payload_type, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .errorunion = &{s} }} }}", .{payload_str}, null);
        },
        // Skip struct/union field names when no name_map
        .struct_type, .union_type => ".{ .id = null, .ty = .{ .scalar = {} } }",
        else => ".{ .id = null, .ty = .{ .scalar = {} } }",
    };
}

/// Look up a non-well-known type in the InternPool.
fn typeToStringLookup(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
    // Check for cycles in recursive types (e.g., struct { next: ?*@This() })
    if (visited.contains(ty)) {
        // Already visiting this type - break the cycle with an unknown placeholder
        return ".{ .id = null, .ty = .{ .scalar = {} } }";
    }
    visited.put(arena, ty, {}) catch @panic("out of memory");

    const type_key = ip.indexToKey(ty);
    return switch (type_key) {
        .simple_type => |simple| switch (simple) {
            .void => ".{ .id = null, .ty = .{ .void = {} } }",
            .noreturn => ".{ .id = null, .ty = .{ .void = {} } }",
            else => ".{ .id = null, .ty = .{ .scalar = {} } }",
        },
        .ptr_type => |ptr| blk: {
            const child_str = typeToStringInner(name_map, field_map, arena, ip, ptr.child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .pointer = &{s} }} }}", .{child_str}, null);
        },
        .opt_type => |child| blk: {
            // opt_type payload is the child type directly (not a struct with .child)
            const child_str = typeToStringInner(name_map, field_map, arena, ip, child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .optional = &{s} }} }}", .{child_str}, null);
        },
        .error_union_type => |eu| blk: {
            // error_union_type has payload_type field
            const payload_str = typeToStringInner(name_map, field_map, arena, ip, eu.payload_type, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .errorunion = &{s} }} }}", .{payload_str}, null);
        },
        .struct_type => structTypeToString(name_map, field_map, arena, ip, ty, visited),
        .union_type => unionTypeToString(name_map, field_map, arena, ip, ty, visited),
        // All other types treated as scalar (int, float, array, enum, etc.)
        else => ".{ .id = null, .ty = .{ .scalar = {} } }",
    };
}

/// Convert a struct type to a Type string with field types and names.
/// New format: .{ .id = TYPE_ID, .ty = .{ .@"struct" = &.{ .{ .id = FIELD_NAME_ID, .ty = TYPE_INNER }, ... } } }
/// The struct's .id is the type_id (derived from InternPool index), used for field name lookup.
/// Each field's .id is the field_name_id for that field's name.
fn structTypeToString(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);

    // Use InternPool index as unique type_id for this struct type
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .id = {d}, .ty = .{{ .@\"struct\" = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - treat deeply nested structs as unknown to avoid complexity
    if (visited.count() > 20) {
        return ".{ .id = null, .ty = .{ .scalar = {} } }";
    }

    // Build field types string - each field is a Type with its own .id (the field name ID)
    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .id = {d}, .ty = .{{ .@\"struct\" = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Get field name and register it using InternPool index
        const field_name_id: u32 = if (loaded.fieldName(ip, i).unwrap()) |name_str| blk: {
            const name_id = @intFromEnum(name_str);
            registerNameWithId(name_map, name_id, name_str.toSlice(ip));
            break :blk name_id;
        } else 0;

        // Register field mapping: {type_id, field_index} -> field_name_id
        if (field_map) |fm| {
            registerFieldName(fm, type_id, @intCast(i), field_name_id);
        }

        // Get the inner type string (without the .id wrapper since we'll add our own)
        const field_type_inner = typeToStringInner(name_map, field_map, arena, ip, field_type, visited);

        // Extract just the .ty = {...} part from the full Type and rebuild with our field name ID
        // typeToStringInner returns ".{ .id = X, .ty = .{ ... } }"
        // We need ".{ .id = FIELD_NAME_ID, .ty = .{ ... } }"
        if (field_name_id != 0) {
            // Replace ".{ .id = N," with ".{ .id = FIELD_NAME_ID,"
            const prefix = clr_allocator.allocPrint(arena, ".{{ .id = {d}", .{field_name_id}, null);
            // Find the position after ".{ .id = " and before the comma
            if (std.mem.indexOf(u8, field_type_inner, ".{ .id = ")) |start| {
                const after_prefix = field_type_inner[start + ".{ .id = ".len ..];
                if (std.mem.indexOf(u8, after_prefix, ",")) |comma_pos| {
                    result.appendSlice(arena, prefix) catch @panic("out of memory");
                    result.appendSlice(arena, after_prefix[comma_pos..]) catch @panic("out of memory");
                } else {
                    result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
                }
            } else {
                result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
            }
        } else {
            result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
        }
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Convert a union type to a Type string with field types.
/// New format: .{ .id = TYPE_ID, .ty = .{ .@"union" = &.{ .{ .id = FIELD_NAME_ID, .ty = TYPE_INNER }, ... } } }
/// The union's .id is the type_id (derived from InternPool index), used for field name lookup.
/// Each field's .id is the field_name_id for that variant's name.
fn unionTypeToString(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadUnionType(type_index);
    const field_types = loaded.field_types.get(ip);

    // Use InternPool index as unique type_id for this union type
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .id = {d}, .ty = .{{ .@\"union\" = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - treat deeply nested unions as unknown
    if (visited.count() > 20) {
        return ".{ .id = null, .ty = .{ .scalar = {} } }";
    }

    // Build field types string - each field is a Type with its own .id (the variant name ID)
    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .id = {d}, .ty = .{{ .@\"union\" = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Get field name from union's tag type and register it using InternPool index
        const field_name_id: u32 = if (getUnionFieldNameInfo(ip, loaded.enum_tag_ty, i)) |info| blk: {
            registerNameWithId(name_map, info.id, info.name);
            break :blk info.id;
        } else 0;

        // Register field mapping: {type_id, field_index} -> field_name_id
        if (field_map) |fm| {
            registerFieldName(fm, type_id, @intCast(i), field_name_id);
        }

        // Get the inner type string
        const field_type_inner = typeToStringInner(name_map, field_map, arena, ip, field_type, visited);

        // Replace existing .id with field_name_id if we have a name
        if (field_name_id != 0) {
            const prefix = clr_allocator.allocPrint(arena, ".{{ .id = {d}", .{field_name_id}, null);
            // Find the position after ".{ .id = " and before the comma
            if (std.mem.indexOf(u8, field_type_inner, ".{ .id = ")) |start| {
                const after_prefix = field_type_inner[start + ".{ .id = ".len ..];
                if (std.mem.indexOf(u8, after_prefix, ",")) |comma_pos| {
                    result.appendSlice(arena, prefix) catch @panic("out of memory");
                    result.appendSlice(arena, after_prefix[comma_pos..]) catch @panic("out of memory");
                } else {
                    result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
                }
            } else {
                result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
            }
        } else {
            result.appendSlice(arena, field_type_inner) catch @panic("out of memory");
        }
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Get field name from union's tag enum type.
/// Info about a union field name: ID (InternPool index) and display name
const UnionFieldNameInfo = struct {
    id: u32, // InternPool index of the name (globally unique)
    name: []const u8, // Display name for error messages
};

fn getUnionFieldNameInfo(ip: *const InternPool, tag_type_idx: InternPool.Index, field_index: usize) ?UnionFieldNameInfo {
    // If tag type is none/invalid, no field names available
    if (tag_type_idx == .none) return null;

    const key = ip.indexToKey(tag_type_idx);
    switch (key) {
        .enum_type => {
            const loaded_enum = ip.loadEnumType(tag_type_idx);
            const names = loaded_enum.names.get(ip);
            if (field_index < names.len) {
                const name_str = names[field_index];
                return .{ .id = @intFromEnum(name_str), .name = name_str.toSlice(ip) };
            }
        },
        else => {},
    }
    return null;
}

/// Convert an aggregate VALUE (not just type) to a Type string with field-level undefined.
/// This inspects each field's value to determine if it's undefined.
/// New format: .{ .id = null, .ty = .{ .@"struct" = &.{ .{ .id = null, .ty = TYPE_OR_UNDEFINED }, ... } } }
fn aggregateValueToString(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, agg: InternPool.Key.Aggregate) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);
    const field_values = agg.storage.values();

    if (field_types.len == 0) {
        return ".{ .id = null, .ty = .{ .@\"struct\" = &.{} } }";
    }

    var visited = VisitedTypes{};

    // Build field types string with undefined info from values
    var result = std.ArrayListUnmanaged(u8){};
    result.appendSlice(arena, ".{ .id = null, .ty = .{ .@\"struct\" = &.{ ") catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Check if this field's value is undefined
        const is_field_undef = if (i < field_values.len) ip.isUndef(field_values[i]) else false;

        const inner_type_str = typeToStringInner(null, null, arena, ip, field_type, &visited);
        // If field is undefined, wrap it in .undefined
        const field_type_str = if (is_field_undef)
            clr_allocator.allocPrint(arena, ".{{ .id = null, .ty = .{{ .undefined = &{s} }} }}", .{inner_type_str}, null)
        else
            inner_type_str;

        result.appendSlice(arena, field_type_str) catch @panic("out of memory");
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

fn payloadStore(info: *const FnInfo, datum: Data) []const u8 {
    // bin_op: lhs = destination ptr, rhs = source value
    const is_undef = isUndefRef(info.ip, datum.bin_op.rhs);
    const ptr: ?usize = if (datum.bin_op.lhs.toIndex()) |idx| @intFromEnum(idx) else null;
    const src_str = if (is_undef)
        // Wrap the type in .undefined when storing undefined
        srcStringUndef(info.arena, info.ip, datum.bin_op.rhs)
    else
        srcString(info.arena, info.ip, datum.bin_op.rhs);
    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {?d}, .src = {s} }}", .{ ptr, src_str }, null);
}

fn payloadLoad(info: *const FnInfo, datum: Data) []const u8 {
    // ty_op.ty is a Ref to the result type
    const ty_str = if (datum.ty_op.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .id = null, .ty = .{ .scalar = {} } }"; // Fallback for unknown types
    // Check for .none first (toIndex asserts on .none)
    if (datum.ty_op.operand == .none) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = null, .ty = {s} }}", .{ty_str}, null);
    }
    // TODO: handle global/interned pointers - for now emit null
    if (datum.ty_op.operand.toIndex()) |idx| {
        const ptr = @intFromEnum(idx);
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {d}, .ty = {s} }}", .{ ptr, ty_str }, null);
    } else {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = null, .ty = {s} }}", .{ty_str}, null);
    }
}

/// ret_ptr returns a pointer to the return value storage.
/// Uses .ty field which contains the pointer type.
fn payloadRetPtr(info: *const FnInfo, datum: Data) []const u8 {
    // datum.ty is the pointer type (e.g., *Container)
    // We need to extract the pointee type for the handler
    const ptr_type = datum.ty.toIntern();
    const ptr_key = info.ip.indexToKey(ptr_type);
    const pointee_type: InternPool.Index = switch (ptr_key) {
        .ptr_type => |p| p.child,
        else => .none,
    };
    if (pointee_type == .none) {
        return ".{ .ty = .{ .unknown = {} } }";
    }
    const ty_str = typeToString(info.name_map, info.field_map, info.arena, info.ip, pointee_type);
    return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s} }}", .{ty_str}, null);
}

/// ret_load loads from ret_ptr to complete return.
/// Uses .un_op field which is the ret_ptr instruction index.
fn payloadRetLoad(info: *const FnInfo, datum: Data) []const u8 {
    // un_op is the operand (ret_ptr instruction)
    if (datum.un_op.toIndex()) |idx| {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {d} }}", .{@intFromEnum(idx)}, null);
    }
    return ".{}";
}

/// Payload for set_union_tag - sets the active tag of a union.
/// Extracts union pointer, field index, and field type from the interned tag value.
fn payloadSetUnionTag(info: *const FnInfo, datum: Data) []const u8 {
    // bin_op: lhs = union ptr, rhs = new tag value (interned enum)
    const ptr: ?usize = if (datum.bin_op.lhs.toIndex()) |idx| @intFromEnum(idx) else null;

    // Extract field index and field type from interned enum tag value
    var field_index: ?u32 = null;
    var field_type: ?InternPool.Index = null;

    const tag_ref = datum.bin_op.rhs;
    if (tag_ref.toInterned()) |interned_idx| {
        const key = info.ip.indexToKey(interned_idx);
        if (key == .enum_tag) {
            // Get the integer value from the enum tag
            const int_idx = key.enum_tag.int;
            const int_key = info.ip.indexToKey(int_idx);
            if (int_key == .int) {
                // Extract the actual integer value
                field_index = switch (int_key.int.storage) {
                    .u64 => |v| @intCast(v),
                    .i64 => |v| @intCast(v),
                    .big_int => |big| big.toInt(u32) catch null,
                    .lazy_align, .lazy_size => null,
                };

                // Try to get field type from the enum's associated union
                const enum_type_idx = key.enum_tag.ty;
                const enum_key = info.ip.indexToKey(enum_type_idx);
                if (enum_key == .enum_type) {
                    // Check if this enum is a generated tag from a union
                    switch (enum_key.enum_type) {
                        .generated_tag => |gt| {
                            // Found the union! Get field type at field_index
                            const union_type_idx = gt.union_type;
                            const loaded_union = info.ip.loadUnionType(union_type_idx);
                            if (field_index) |idx| {
                                const field_types = loaded_union.field_types.get(info.ip);
                                if (idx < field_types.len) {
                                    field_type = field_types[idx];
                                }
                            }
                        },
                        else => {},
                    }
                }
            }
        }
    }

    const ty_str = if (field_type) |ft|
        typeToString(null, null, info.arena, info.ip, ft)
    else
        ".{ .id = null, .ty = .{ .scalar = {} } }"; // Fallback for untagged unions or errors

    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {?d}, .field_index = {?d}, .ty = {s} }}", .{ ptr, field_index, ty_str }, null);
}

/// Payload for get_union_tag - gets the active tag from a union.
/// Uses ty_op: operand is the union value.
fn payloadGetUnionTag(info: *const FnInfo, datum: Data) []const u8 {
    const operand: ?usize = if (datum.ty_op.operand.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(info.arena, ".{{ .operand = {?d} }}", .{operand}, null);
}

fn payloadDbg(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.pl_op.operand;
    const name_index = datum.pl_op.payload;

    // Extract the pointer instruction being named
    const ptr: ?usize = if (operand.toIndex()) |idx| @intFromEnum(idx) else null;

    // Extract the variable name from extra as NullTerminatedString
    const name = extractString(info.extra, name_index);

    // Register the name and get its ID for interned lookup
    const name_id = registerName(info.name_map, name);

    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {?d}, .name_id = {d} }}", .{ ptr, name_id }, null);
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

fn payloadCallParts(info: *const FnInfo, datum: Data) CallParts {
    // Call uses pl_op: operand is callee, payload indexes into extra
    // extra[payload] is args_len, followed by args_len Refs
    const callee_ref = datum.pl_op.operand;
    const payload_index = datum.pl_op.payload;
    const args_len = info.extra[payload_index];

    // Get called function - look up the instruction that produces the callee
    const called_str = if (callee_ref.toIndex()) |idx| blk: {
        // Indirect call through function pointer (load, slice_elem_val, etc.)
        const callee_idx = @intFromEnum(idx);
        const callee_tag = info.tags[callee_idx];
        _ = callee_tag;
        break :blk "null"; // TODO: handle indirect calls
    } else if (callee_ref.toInterned()) |ip_idx| blk: {
        // Direct call to known function - use InternPool index as func_index
        break :blk clr_allocator.allocPrint(info.arena, "fn_{d}", .{@intFromEnum(ip_idx)}, null);
    } else "null";

    // Build args tuple string: .{ Arg, Arg, ... }
    // Args are tagged unions: .{ .eidx = N } or .{ .interned = Type }
    var args_str: []const u8 = ".{";
    var first = true;

    var i: u32 = 0;
    while (i < args_len) : (i += 1) {
        const arg_ref: Ref = @enumFromInt(info.extra[payload_index + 1 + i]);
        if (arg_ref.toIndex()) |idx| {
            // Runtime value from local instruction - look up entity index from results
            const inst_idx = @intFromEnum(idx);
            if (first) {
                args_str = clr_allocator.allocPrint(info.arena, "{s} Arg{{ .eidx = state.results[{d}].refinement.? }}", .{ args_str, inst_idx }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(info.arena, "{s}, Arg{{ .eidx = state.results[{d}].refinement.? }}", .{ args_str, inst_idx }, null);
            }
        } else if (arg_ref.toInterned()) |interned_idx| {
            // Skip zero-sized types - they have no runtime representation
            const val_type = info.ip.typeOf(interned_idx);
            if (isZeroSizedType(info.ip, val_type)) continue;
            // Interned constant - pass type info so runtime can create entity
            const type_str = typeToString(null, null, info.arena, info.ip, val_type);
            if (first) {
                args_str = clr_allocator.allocPrint(info.arena, "{s} Arg{{ .interned = {s} }}", .{ args_str, type_str }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(info.arena, "{s}, Arg{{ .interned = {s} }}", .{ args_str, type_str }, null);
            }
        }
    }
    args_str = clr_allocator.allocPrint(info.arena, "{s} }}", .{args_str}, null);

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

/// Get the result type of an AIR instruction by its index.
/// Returns null if the type can't be determined.
fn getInstResultType(_: *const InternPool, tags: []const Tag, data: []const Data, inst_idx: usize) ?InternPool.Index {
    if (inst_idx >= tags.len or inst_idx >= data.len) return null;
    const tag = tags[inst_idx];

    // Read the data safely
    const datum = data[inst_idx];

    return switch (tag) {
        // Tags that store result type in ty_op.ty
        .alloc,
        .bitcast,
        .load,
        .struct_field_ptr_index_0,
        .struct_field_ptr_index_1,
        .struct_field_ptr_index_2,
        .struct_field_ptr_index_3,
        => blk: {
            // Read raw bytes to avoid DLL relocation issues with union field access
            const raw_ptr: [*]const u32 = @ptrCast(&datum);
            const ty_raw: u32 = raw_ptr[0];
            const ty_ref: Ref = @enumFromInt(ty_raw);
            break :blk ty_ref.toInterned();
        },
        else => null,
    };
}

/// Get the container type for a struct/union field access operation.
/// The operand is a pointer to the container - we get its type and dereference it.
fn getContainerType(ip: *const InternPool, tags: []const Tag, data: []const Data, operand: Ref) ?InternPool.Index {
    // If operand is interned, get its type directly
    if (operand.toInterned()) |interned_idx| {
        const ty = ip.typeOf(interned_idx);
        const type_key = ip.indexToKey(ty);
        return switch (type_key) {
            .ptr_type => |pt| pt.child, // Dereference pointer to get container
            else => null,
        };
    }
    // If operand is an instruction, get its result type
    if (operand.toIndex()) |idx| {
        const ptr_type = getInstResultType(ip, tags, data, @intFromEnum(idx)) orelse return null;
        const type_key = ip.indexToKey(ptr_type);
        return switch (type_key) {
            .ptr_type => |pt| pt.child, // Dereference pointer to get container
            else => null,
        };
    }
    return null;
}

/// Get the field name ID for a field access operation (struct_field_ptr).
/// The operand is a pointer to the container - we dereference it.
/// Returns 0 if the container type cannot be traced.
fn getFieldNameId(info: *const FnInfo, operand: Ref, field_index: usize) u32 {
    // First check for interned operand (comptime known)
    if (operand.toInterned()) |interned_idx| {
        const ty = info.ip.typeOf(interned_idx);
        const type_key = info.ip.indexToKey(ty);
        const container = switch (type_key) {
            .ptr_type => |pt| pt.child,
            else => return 0,
        };
        return lookupFieldName(info.name_map, info.ip, container, field_index);
    }

    // Operand is an instruction reference
    if (operand.toIndex()) |_| {
        const container = getContainerType(info.ip, info.tags, info.data, operand) orelse return 0;
        return lookupFieldName(info.name_map, info.ip, container, field_index);
    }

    return 0;
}

/// Get the field name ID for a field access operation (struct_field_val).
/// The operand is the container value itself (not a pointer).
/// Returns 0 if the container type cannot be traced.
fn getFieldNameIdFromValue(info: *const FnInfo, operand: Ref, field_index: usize) u32 {
    // If operand is interned, get its type directly (it IS the container)
    if (operand.toInterned()) |interned_idx| {
        const container = info.ip.typeOf(interned_idx);
        return lookupFieldName(info.name_map, info.ip, container, field_index);
    }

    // Operand is an instruction reference - get its result type directly
    if (operand.toIndex()) |idx| {
        const container = getInstResultType(info.ip, info.tags, info.data, @intFromEnum(idx)) orelse return 0;
        return lookupFieldName(info.name_map, info.ip, container, field_index);
    }

    return 0;
}

/// Look up field name from a container type (struct or union)
/// Returns 0 if the container type is invalid or not a struct/union.
fn lookupFieldName(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), ip: *const InternPool, container_type: InternPool.Index, field_index: usize) u32 {
    // Validate container_type is within reasonable bounds
    // InternPool indices shouldn't be extremely large values like 0x10000000
    if (@intFromEnum(container_type) > 100000) return 0;

    const type_key = ip.indexToKey(container_type);
    return switch (type_key) {
        .struct_type => blk: {
            const loaded = ip.loadStructType(container_type);
            const name_str = loaded.fieldName(ip, field_index).unwrap() orelse break :blk 0;
            const name_id = @intFromEnum(name_str);
            registerNameWithId(name_map, name_id, name_str.toSlice(ip));
            break :blk name_id;
        },
        .union_type => blk: {
            const loaded = ip.loadUnionType(container_type);
            const info = getUnionFieldNameInfo(ip, loaded.enum_tag_ty, field_index) orelse break :blk 0;
            registerNameWithId(name_map, info.id, info.name);
            break :blk info.id;
        },
        else => 0,
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

/// Generate parameter list string like "arg0: Arg, arg1: Arg"
fn buildParamList(arena: std.mem.Allocator, arg_count: u32) []const u8 {
    if (arg_count == 0) return "";

    var result: []const u8 = clr_allocator.allocPrint(arena, "arg0: Arg", .{}, null);
    var i: u32 = 1;
    while (i < arg_count) : (i += 1) {
        result = clr_allocator.allocPrint(arena, "{s}, arg{d}: Arg", .{ result, i }, null);
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

/// Info about an allocator type: both the ID (for unique tracking) and display name (for errors)
const AllocatorTypeInfo = struct {
    id: u32, // InternPool index of nav.fqn (globally unique) or hash for runtime allocators
    name: []const u8, // Display name for error messages (e.g., "PageAllocator")
};

/// Extract allocator type from the first argument to create/destroy
/// Returns InternPool index of nav.fqn (for uniqueness) and display name (for error messages)
fn extractAllocatorType(ip: *const InternPool, datum: Data, extra: []const u32, tags: []const Tag, data: []const Data) AllocatorTypeInfo {
    const payload_index = datum.pl_op.payload;
    const args_len = extra[payload_index];
    if (args_len == 0) return .{ .id = 0, .name = "unknown" };

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
                                                // Use InternPool index of fqn as globally unique ID
                                                const fqn_id = @intFromEnum(nav.fqn);
                                                const fqn = nav.fqn.toSlice(ip);
                                                // FQN is like "heap.PageAllocator.vtable"
                                                // Extract "PageAllocator" for display
                                                return .{ .id = fqn_id, .name = extractAllocatorName(fqn) };
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
        return .{ .id = 0, .name = "comptime_unknown" };
    }

    // It's an inst_ref - trace to find source (use hash for runtime allocators)
    if (arg_ref.toIndex()) |inst_idx| {
        const name = traceAllocatorType(tags, data, ip, @intFromEnum(inst_idx), 0);
        // Use hash with high bit set for runtime allocators (no InternPool index available)
        const hash = std.hash.Wyhash.hash(0, name);
        return .{ .id = @as(u32, @truncate(hash | 0x80000000)), .name = name };
    }

    return .{ .id = 0, .name = "unknown" };
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
        .struct_field_ptr_index_0, .struct_field_ptr_index_1, .struct_field_ptr_index_2, .struct_field_ptr_index_3 => {
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

const SubTag = union(enum) {
    cond_br_true,
    cond_br_false,
    switch_case: struct {
        case_index: u32,
        num_cases: u32,
        /// If switching on a union tag, this contains the union inst and field index
        union_tag: ?UnionTagCheck = null,
    },
};

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
            .sub => |s| switch (s.tag) {
                .cond_br_true => clr_allocator.allocPrint(arena, "fn_{d}_cond_br_true_{d}", .{
                    s.func_index,
                    s.instr_index,
                }, null),
                .cond_br_false => clr_allocator.allocPrint(arena, "fn_{d}_cond_br_false_{d}", .{
                    s.func_index,
                    s.instr_index,
                }, null),
                .switch_case => |sc| clr_allocator.allocPrint(arena, "fn_{d}_switch_case_{d}_{d}", .{
                    s.func_index,
                    sc.case_index,
                    s.instr_index,
                }, null),
            },
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
                \\    defer refinements.testValid();
                \\
                \\    const results = try Inst.make_results_list(ctx.allocator, {d});
                \\    defer Inst.clear_results_list(results, ctx.allocator);
                \\    const return_eidx: EIdx = if (caller_refinements) |cp| try cp.appendEntity(.{{ .retval_future = {{}} }}) else 0;
                \\
                \\    const state = State{{ .ctx = ctx, .results = results, .refinements = &refinements, .return_eidx = return_eidx, .caller_refinements = caller_refinements }};
                \\
                \\
            , .{ f.func_index, params, file_path, base_line, fqn, num_insts }, null),
            .sub => |s| switch (s.tag) {
                .cond_br_true => clr_allocator.allocPrint(arena,
                    \\fn fn_{d}_cond_br_true_{d}(state: State) anyerror!void {{
                    \\
                , .{ s.func_index, s.instr_index }, null),
                .cond_br_false => clr_allocator.allocPrint(arena,
                    \\fn fn_{d}_cond_br_false_{d}(state: State) anyerror!void {{
                    \\
                , .{ s.func_index, s.instr_index }, null),
                .switch_case => |sc| clr_allocator.allocPrint(arena,
                    \\fn fn_{d}_switch_case_{d}_{d}(state: State) anyerror!void {{
                    \\
                , .{ s.func_index, sc.case_index, s.instr_index }, null),
            },
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

/// Switch case info for extraction
const SwitchCaseInfo = struct {
    body: []const u32,
    /// Field index if this case matches an enum tag (for union switch)
    field_index: ?u32 = null,
};

/// Result of extracting switch bodies
const SwitchBodies = struct {
    cases: []const SwitchCaseInfo,
    else_body: []const u32,
    /// If switching on a union tag, this is the union operand instruction
    union_operand: ?u32 = null,
};

/// Extract case and else body indices from a switch_br instruction.
/// Also detects if switching on union tag and extracts field indices.
/// Returns null if extraction fails.
fn extractSwitchBrBodies(arena: std.mem.Allocator, ip: *const InternPool, switch_idx: usize, tags: []const Tag, data: []const Data, extra: []const u32) ?SwitchBodies {
    if (switch_idx >= data.len) return null;
    const switch_datum = data[switch_idx];
    const payload_index = switch_datum.pl_op.payload;
    if (payload_index + 2 > extra.len) return null;

    const cases_len = extra[payload_index];
    const else_body_len = extra[payload_index + 1];

    // Check if operand is get_union_tag - if so, this is a union switch
    var union_operand: ?u32 = null;
    const operand_ref = switch_datum.pl_op.operand;
    if (operand_ref.toIndex()) |operand_idx| {
        const operand_idx_u32: u32 = @intFromEnum(operand_idx);
        if (operand_idx_u32 < tags.len and tags[operand_idx_u32] == .get_union_tag) {
            // This is a switch on union tag - get the union operand
            const get_tag_datum = data[operand_idx_u32];
            if (get_tag_datum.ty_op.operand.toIndex()) |union_idx| {
                union_operand = @intFromEnum(union_idx);
            }
        }
    }

    // Calculate branch hints size (packed 10 per u32)
    const hint_bag_count = (cases_len + 1 + 9) / 10; // ceil((cases_len + 1) / 10)
    var extra_index: u32 = payload_index + 2 + @as(u32, @intCast(hint_bag_count));

    // Parse each case
    var cases = arena.alloc(SwitchCaseInfo, cases_len) catch return null;
    for (0..cases_len) |i| {
        if (extra_index + 3 > extra.len) return null;
        const items_len = extra[extra_index];
        const ranges_len = extra[extra_index + 1];
        const body_len = extra[extra_index + 2];
        extra_index += 3;

        // Extract field_index from first item if this is a union switch
        var field_index: ?u32 = null;
        if (union_operand != null and items_len > 0) {
            // Items are Ref values stored as u32s
            const item_ref: Ref = @enumFromInt(extra[extra_index]);
            if (item_ref.toInterned()) |interned_idx| {
                field_index = extractEnumTagFieldIndex(ip, interned_idx);
            }
        }

        // Skip items and ranges
        const items_start = extra_index;
        _ = items_start;
        extra_index += items_len;
        extra_index += ranges_len * 2; // ranges are pairs

        // Extract body
        if (extra_index + body_len > extra.len) return null;
        cases[i] = .{ .body = extra[extra_index..][0..body_len], .field_index = field_index };
        extra_index += body_len;
    }

    // Extract else body
    if (extra_index + else_body_len > extra.len) return null;
    const else_body = extra[extra_index..][0..else_body_len];

    // Note: We intentionally don't expand else into separate cases for union switches.
    // While it would provide more precise variant tracking, the else body typically
    // contains safety checks with nested cond_br, and sharing the same body across
    // multiple expanded cases causes duplicate function name collisions.
    // Explicit cases get full variant tracking; else cases do not.

    return .{
        .cases = cases,
        .else_body = else_body,
        .union_operand = union_operand,
    };
}

/// Extract the field index from an interned enum tag value
fn extractEnumTagFieldIndex(ip: *const InternPool, interned_idx: InternPool.Index) ?u32 {
    const key = ip.indexToKey(interned_idx);
    if (key != .enum_tag) return null;

    const int_idx = key.enum_tag.int;
    const int_key = ip.indexToKey(int_idx);
    if (int_key != .int) return null;

    return switch (int_key.int.storage) {
        .u64 => |v| @intCast(v),
        .i64 => |v| @intCast(v),
        .big_int => |big| big.toInt(u32) catch null,
        .lazy_align, .lazy_size => null,
    };
}

/// Union tag check info for variant safety
const UnionTagCheck = struct {
    union_inst: usize, // instruction index that holds the union
    field_index: u32, // the variant field being checked
};

/// Check if the condition is a union tag comparison (get_union_tag + cmp_eq).
/// Returns the union instruction and field_index if it's a tag check pattern.
fn getUnionTagCheck(ip: *const InternPool, condition_idx: usize, tags: []const Tag, data: []const Data) ?UnionTagCheck {
    // Check if condition is cmp_eq
    if (tags[condition_idx] != .cmp_eq) return null;

    const cmp_datum = data[condition_idx];
    const lhs_ref = cmp_datum.bin_op.lhs;
    const rhs_ref = cmp_datum.bin_op.rhs;

    // Check if lhs is get_union_tag (instruction) and rhs is interned enum tag
    const lhs_idx = lhs_ref.toIndex() orelse return null;
    const lhs_idx_u32: u32 = @intFromEnum(lhs_idx);

    if (tags[lhs_idx_u32] != .get_union_tag) return null;

    // Get the union instruction from get_union_tag's operand
    const get_tag_datum = data[lhs_idx_u32];
    const union_ref = get_tag_datum.ty_op.operand;
    const union_idx = union_ref.toIndex() orelse return null;

    // Extract field_index from rhs interned enum tag
    const interned_idx = rhs_ref.toInterned() orelse return null;
    const key = ip.indexToKey(interned_idx);
    if (key != .enum_tag) return null;

    const int_idx = key.enum_tag.int;
    const int_key = ip.indexToKey(int_idx);
    if (int_key != .int) return null;

    const field_index: ?u32 = switch (int_key.int.storage) {
        .u64 => |v| @intCast(v),
        .i64 => |v| @intCast(v),
        .big_int => |big| big.toInt(u32) catch null,
        .lazy_align, .lazy_size => null,
    };

    return .{
        .union_inst = @intFromEnum(union_idx),
        .field_index = field_index orelse return null,
    };
}

/// Check if this cond_br is part of the .? (optional unwrap) pattern.
/// The .? operator in debug/safe modes injects a runtime null check:
/// is_non_null + cond_br(call unwrapNull + unreach).
/// We detect this pattern to noop the compiler-injected safety check,
/// allowing our static null detection via optional_payload to work correctly.
///
/// Pattern: condition is is_non_null, else branch contains call to unwrapNull
fn isDotQPattern(ip: *const InternPool, cond_br_idx: usize, tags: []const Tag, data: []const Data, extra: []const u32) bool {
    // 1. Get condition operand from cond_br
    const cond_br_datum = data[cond_br_idx];
    const condition_ref = cond_br_datum.pl_op.operand;
    const condition_idx = condition_ref.toIndex() orelse return false;
    const condition_idx_u32: u32 = @intFromEnum(condition_idx);

    // 2. Check if condition is is_non_null
    if (tags[condition_idx_u32] != .is_non_null) return false;

    // 3. Extract else branch body
    const bodies = extractCondBrBodies(cond_br_idx, data, extra) orelse return false;

    // 4. Check else branch for unwrapNull call
    for (bodies.@"else") |else_idx| {
        const else_tag = tags[else_idx];
        if (else_tag == .call or else_tag == .call_always_tail or
            else_tag == .call_never_tail or else_tag == .call_never_inline)
        {
            const fqn = getCallFqn(ip, data[else_idx]) orelse continue;
            if (std.mem.endsWith(u8, fqn, "unwrapNull")) {
                return true;
            }
        }
    }
    return false;
}

/// Generate Zig source code for a function from AIR instructions using worklist algorithm.
/// This processes the main function and any nested conditionals, outputting branch functions
/// first (so they're in scope) followed by the main function.
pub fn generateFunction(func_index: u32, fqn: []const u8, info: *const FnInfo, base_line: u32, file_path: []const u8) []u8 {
    if (info.tags.len == 0) @panic("function with no instructions encountered");

    // Count args and build parameter list (only for full functions)
    const arg_count = countArgs(info.tags);
    const param_list = buildParamList(info.arena, arg_count);
    const params: []const u8 = if (arg_count > 0)
        clr_allocator.allocPrint(info.arena, ", {s}", .{param_list}, null)
    else
        "";

    // Worklist of functions to generate
    var worklist: std.ArrayListUnmanaged(FunctionGen) = .empty;
    // Output: generated functions (sub functions first, main last)
    var sub_functions: std.ArrayListUnmanaged([]const u8) = .empty;
    var sub_functions_len: usize = 0;

    // Start with the main function
    worklist.append(info.arena, .{ .full = .{ .func_index = func_index } }) catch @panic("out of memory");

    var main_function: []const u8 = "";

    while (worklist.items.len > 0) {
        const item = worklist.pop().?;
        // Generate this function
        const func_str = generateOneFunction(
            info,
            item,
            func_index,
            fqn,
            params,
            file_path,
            base_line,
            &worklist,
        );

        switch (item) {
            .full => {
                main_function = func_str;
            },
            .sub => {
                sub_functions.append(info.arena, func_str) catch @panic("out of memory");
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
    info: *const FnInfo,
    item: FunctionGen,
    func_index: u32,
    fqn: []const u8,
    params: []const u8,
    file_path: []const u8,
    base_line: u32,
    worklist: *std.ArrayListUnmanaged(FunctionGen),
) []const u8 {
    // Build body_set: the set of instruction indices that are in this function's body.
    // Instructions not in this set get noop (they're unreferenced/internal structures).
    // We iteratively expand to include block bodies (blocks are inlined, not separate functions).
    var body_set: std.AutoHashMapUnmanaged(u32, void) = .empty;
    var pending: std.ArrayListUnmanaged(u32) = .empty;

    // Start with initial body indices
    switch (item) {
        .full => {
            // Main body from extra data (mirrors Air.getMainBody())
            // extra[0] = block_index, points to Block structure
            // Block structure: body_len (u32), followed by body_len instruction indices
            const block_index = info.extra[0];
            const body_len = info.extra[block_index];
            for (info.extra[block_index + 1 ..][0..body_len]) |body_idx| {
                pending.append(info.arena, body_idx) catch @panic("out of memory");
            }
        },
        .sub => |s| {
            // Sub-function body indices passed from parent
            for (s.body_indices) |body_idx| {
                pending.append(info.arena, body_idx) catch @panic("out of memory");
            }
        },
    }

    // Process pending indices, expanding block bodies as we find them
    while (pending.items.len > 0) {
        const idx = pending.pop().?;
        if (body_set.contains(idx)) continue;
        body_set.put(info.arena, idx, {}) catch @panic("out of memory");

        // If this is a block, add its body to pending (blocks are inlined)
        if (info.tags[idx] == .block) {
            if (extractBlockBody(idx, info.data, info.extra)) |block_body| {
                for (block_body) |body_idx| {
                    pending.append(info.arena, body_idx) catch @panic("out of memory");
                }
            }
        }
    }

    // Stack for instruction data (will be reversed)
    const InstrData = struct {
        idx: u32,
        tag: Tag,
        datum: Data,
        is_noop: bool,
    };
    var stack: std.ArrayListUnmanaged(InstrData) = .empty;

    // For full functions: iterate ALL indices 0..tags.len, emit noop for non-body indices
    // For sub functions: only iterate body indices (no noops needed outside body)
    switch (item) {
        .full => {
            var i: usize = info.tags.len;
            while (i > 0) {
                i -= 1;
                const idx: u32 = @intCast(i);

                if (!body_set.contains(idx)) {
                    // Not in body - emit noop
                    stack.append(info.arena, .{ .idx = idx, .tag = undefined, .datum = undefined, .is_noop = true }) catch @panic("out of memory");
                    continue;
                }

                const tag = info.tags[idx];
                const datum = info.data[idx];

                // Check for cond_br - add branches to worklist (unless it's a .? pattern)
                var is_dotq_noop = false;
                if (tag == .cond_br) {
                    if (isDotQPattern(info.ip, idx, info.tags, info.data, info.extra)) {
                        // .? pattern detected - noop the cond_br, skip branch functions
                        is_dotq_noop = true;
                        // Don't add branch functions to worklist
                    } else if (extractCondBrBodies(idx, info.data, info.extra)) |bodies| {
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .cond_br_true,
                            .body_indices = bodies.then,
                        } }) catch @panic("out of memory");
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .cond_br_false,
                            .body_indices = bodies.@"else",
                        } }) catch @panic("out of memory");
                    }
                }

                // Check for switch_br - add all cases to worklist
                if (tag == .switch_br) {
                    if (extractSwitchBrBodies(info.arena, info.ip, idx, info.tags, info.data, info.extra)) |bodies| {
                        const has_else = bodies.else_body.len > 0;
                        const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                        // Add each case
                        for (bodies.cases, 0..) |case, case_idx| {
                            // Build union_tag info if this is a union switch
                            const union_tag: ?UnionTagCheck = if (bodies.union_operand != null and case.field_index != null)
                                .{ .union_inst = bodies.union_operand.?, .field_index = case.field_index.? }
                            else
                                null;
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .switch_case = .{
                                    .case_index = @intCast(case_idx),
                                    .num_cases = num_cases,
                                    .union_tag = union_tag,
                                } },
                                .body_indices = case.body,
                            } }) catch @panic("out of memory");
                        }
                        // Add else case only if not expanded into specific cases
                        if (has_else) {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .switch_case = .{
                                    .case_index = @intCast(bodies.cases.len),
                                    .num_cases = num_cases,
                                    .union_tag = null,
                                } },
                                .body_indices = bodies.else_body,
                            } }) catch @panic("out of memory");
                        }
                    }
                }

                stack.append(info.arena, .{ .idx = idx, .tag = tag, .datum = datum, .is_noop = is_dotq_noop }) catch @panic("out of memory");
            }
        },
        .sub => {
            // For sub functions, only process body_set indices (which includes block expansions)
            // No noops needed - parent function handles indices outside our body
            // Collect and sort body_set keys to iterate in order
            var body_indices_list: std.ArrayListUnmanaged(u32) = .empty;
            var it = body_set.iterator();
            while (it.next()) |entry| {
                body_indices_list.append(info.arena, entry.key_ptr.*) catch @panic("out of memory");
            }
            // Sort in descending order for stack (will be reversed when popped)
            std.mem.sort(u32, body_indices_list.items, {}, std.sort.desc(u32));

            for (body_indices_list.items) |idx| {
                const tag = info.tags[idx];
                const datum = info.data[idx];

                // Check for nested cond_br (unless it's a .? pattern)
                var is_dotq_noop = false;
                if (tag == .cond_br) {
                    if (isDotQPattern(info.ip, idx, info.tags, info.data, info.extra)) {
                        // .? pattern detected - noop the cond_br, skip branch functions
                        is_dotq_noop = true;
                    } else if (extractCondBrBodies(idx, info.data, info.extra)) |bodies| {
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .cond_br_true,
                            .body_indices = bodies.then,
                        } }) catch @panic("out of memory");
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .cond_br_false,
                            .body_indices = bodies.@"else",
                        } }) catch @panic("out of memory");
                    }
                }

                // Check for nested switch_br
                if (tag == .switch_br) {
                    if (extractSwitchBrBodies(info.arena, info.ip, idx, info.tags, info.data, info.extra)) |bodies| {
                        const has_else = bodies.else_body.len > 0;
                        const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                        for (bodies.cases, 0..) |case, case_idx| {
                            const union_tag: ?UnionTagCheck = if (bodies.union_operand != null and case.field_index != null)
                                .{ .union_inst = bodies.union_operand.?, .field_index = case.field_index.? }
                            else
                                null;
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .switch_case = .{
                                    .case_index = @intCast(case_idx),
                                    .num_cases = num_cases,
                                    .union_tag = union_tag,
                                } },
                                .body_indices = case.body,
                            } }) catch @panic("out of memory");
                        }
                        if (has_else) {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .switch_case = .{
                                    .case_index = @intCast(bodies.cases.len),
                                    .num_cases = num_cases,
                                    .union_tag = null,
                                } },
                                .body_indices = bodies.else_body,
                            } }) catch @panic("out of memory");
                        }
                    }
                }

                stack.append(info.arena, .{ .idx = idx, .tag = tag, .datum = datum, .is_noop = is_dotq_noop }) catch @panic("out of memory");
            }
        },
    }

    // Pop from stack to generate lines in forward order
    var lines: std.ArrayListUnmanaged([]const u8) = .empty;
    var total_len: usize = 0;
    var arg_counter: u32 = 0;

    // For sub functions, inject tag line at the start for refinement
    switch (item) {
        .sub => |s| switch (s.tag) {
            .cond_br_true, .cond_br_false => {
                // Get the condition operand from the cond_br instruction
                const cond_br_datum = info.data[s.instr_index];
                const condition_ref = cond_br_datum.pl_op.operand;
                const condition_idx: ?usize = if (condition_ref.toIndex()) |idx| @intFromEnum(idx) else null;
                const is_true_branch = s.tag == .cond_br_true;

                // Check if this is a union tag comparison pattern
                const union_tag: ?UnionTagCheck = if (condition_idx) |cond_idx|
                    getUnionTagCheck(info.ip, cond_idx, info.tags, info.data)
                else
                    null;

                const cond_br_line = generateCondBrTagLine(info.arena, is_true_branch, condition_idx, union_tag);
                lines.append(info.arena, cond_br_line) catch @panic("out of memory");
                total_len += cond_br_line.len;
            },
            .switch_case => |sc| {
                // Inject SwitchBr tag at the start of switch case
                const switch_br_line = generateSwitchBrTagLine(info.arena, sc.case_index, sc.num_cases, sc.union_tag);
                lines.append(info.arena, switch_br_line) catch @panic("out of memory");
                total_len += switch_br_line.len;
            },
        },
        .full => {},
    }

    while (stack.items.len > 0) {
        const instr = stack.pop().?;
        var line: []const u8 = undefined;

        if (instr.is_noop) {
            // Generate noop for unreferenced instruction
            line = generateNoopLine(info.arena, instr.idx);
        } else if (instr.tag == .cond_br) {
            // Generate Inst.cond_br call with function references
            line = generateCondBrLine(info.arena, instr.idx, func_index);
        } else if (instr.tag == .switch_br) {
            // Generate Inst.switch_br call with all case function references
            if (extractSwitchBrBodies(info.arena, info.ip, instr.idx, info.tags, info.data, info.extra)) |bodies| {
                const has_else = bodies.else_body.len > 0;
                const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                line = generateSwitchBrLine(info.arena, instr.idx, func_index, num_cases);
            } else {
                // Fallback to noop if extraction fails
                line = generateNoopLine(info.arena, instr.idx);
            }
        } else {
            line = _instLine(info, instr.tag, instr.datum, instr.idx, &arg_counter);
        }

        lines.append(info.arena, line) catch @panic("out of memory");
        total_len += line.len;
    }

    // Concatenate all lines
    const body_lines = info.arena.alloc(u8, total_len) catch @panic("out of memory");
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
                if (info.tags[body_idx] == .ret_safe) break :blk false;
            }
            break :blk true;
        },
    };

    // Generate complete function with header/footer
    const header_str = item.header(info.arena, params, fqn, file_path, base_line, info.tags.len, discard_caller_params);
    const footer_str = item.footer(info.arena);

    const func_len = header_str.len + body_lines.len + footer_str.len;
    const func_buf = info.arena.alloc(u8, func_len) catch @panic("out of memory");
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

/// Generate noop line for unreferenced instructions (gaps in the AIR array)
fn generateNoopLine(arena: std.mem.Allocator, idx: u32) []const u8 {
    return clr_allocator.allocPrint(arena,
        \\    try Inst.apply(state, {d}, .{{ .noop = .{{}} }});
        \\
    , .{idx}, null);
}

/// Generate CondBr tag line at the start of branch functions for null_safety refinement
fn generateCondBrTagLine(arena: std.mem.Allocator, is_true_branch: bool, condition_idx: ?usize, union_tag: ?UnionTagCheck) []const u8 {
    // Only emit union_tag if it's set (defaults to null in tag.zig)
    if (union_tag) |ut| {
        return clr_allocator.allocPrint(arena,
            \\    try Inst.apply(state, 0, .{{ .cond_br = .{{ .branch = {}, .condition_idx = {?d}, .union_tag = .{{ .union_inst = {d}, .field_index = {d} }} }} }});
            \\
        , .{ is_true_branch, condition_idx, ut.union_inst, ut.field_index }, null);
    } else {
        return clr_allocator.allocPrint(arena,
            \\    try Inst.apply(state, 0, .{{ .cond_br = .{{ .branch = {}, .condition_idx = {?d} }} }});
            \\
        , .{ is_true_branch, condition_idx }, null);
    }
}

/// Generate Inst.switch_br call with all case function references.
/// The function builds a tuple of function pointers for all cases.
fn generateSwitchBrLine(arena: std.mem.Allocator, switch_idx: u32, func_index: u32, num_cases: u32) []const u8 {
    // Build the tuple of function pointers: .{ fn_X_switch_case_0_Y, fn_X_switch_case_1_Y, ... }
    var case_fns: []const u8 = ".{";
    for (0..num_cases) |i| {
        if (i > 0) {
            case_fns = clr_allocator.allocPrint(arena, "{s}, fn_{d}_switch_case_{d}_{d}", .{
                case_fns,
                func_index,
                i,
                switch_idx,
            }, null);
        } else {
            case_fns = clr_allocator.allocPrint(arena, "{s} fn_{d}_switch_case_{d}_{d}", .{
                case_fns,
                func_index,
                i,
                switch_idx,
            }, null);
        }
    }
    case_fns = clr_allocator.allocPrint(arena, "{s} }}", .{case_fns}, null);

    return clr_allocator.allocPrint(arena,
        \\    try Inst.switch_br(state, {d}, {s});
        \\
    , .{ switch_idx, case_fns }, null);
}

/// Generate SwitchBr tag line at the start of switch case functions
fn generateSwitchBrTagLine(arena: std.mem.Allocator, case_index: u32, num_cases: u32, union_tag: ?UnionTagCheck) []const u8 {
    if (union_tag) |ut| {
        return clr_allocator.allocPrint(arena,
            \\    try Inst.apply(state, 0, .{{ .switch_br = .{{ .case_index = {d}, .num_cases = {d}, .union_tag = .{{ .union_inst = {d}, .field_index = {d} }} }} }});
            \\
        , .{ case_index, num_cases, ut.union_inst, ut.field_index }, null);
    } else {
        return clr_allocator.allocPrint(arena,
            \\    try Inst.apply(state, 0, .{{ .switch_br = .{{ .case_index = {d}, .num_cases = {d} }} }});
            \\
        , .{ case_index, num_cases }, null);
    }
}

/// Generate the getName function that maps field IDs to field names.
pub fn emitGetName(name_map: *std.AutoHashMapUnmanaged(u32, []const u8)) []const u8 {
    const allocator = clr_allocator.allocator();

    // Build switch arms
    var arms = std.ArrayListUnmanaged(u8){};

    var it = name_map.iterator();
    while (it.next()) |entry| {
        const id = entry.key_ptr.*;
        const name = entry.value_ptr.*;
        const arm = clr_allocator.allocPrint(allocator, "        {d} => \"{s}\",\n", .{ id, name }, null);
        arms.appendSlice(allocator, arm) catch @panic("OOM");
    }

    // Build final function
    return clr_allocator.allocPrint(allocator,
        \\pub fn getName(id: u32) []const u8 {{
        \\    return switch (id) {{
        \\{s}        else => "unknown",
        \\    }};
        \\}}
        \\
    , .{arms.items}, null);
}

/// Field entry for grouping in emitGetFieldId
const FieldEntry = struct { field_index: u32, name_id: u32 };

/// Generate getFieldId function from combined field mappings.
/// Uses array lookup for field_index within each type_id.
pub fn emitGetFieldId(field_map: *clr.FieldHashMap) []const u8 {
    const allocator = clr_allocator.allocator();

    if (field_map.count() == 0) {
        return
            \\pub fn getFieldId(type_id: u32, field_index: u32) ?u32 {
            \\    _ = type_id;
            \\    _ = field_index;
            \\    return null;
            \\}
            \\
        ;
    }

    // Group mappings by type_id, tracking max field_index for each type
    var type_groups = std.AutoHashMapUnmanaged(u32, std.ArrayListUnmanaged(FieldEntry)){};

    var it = field_map.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const name_id = entry.value_ptr.*;

        const gop = type_groups.getOrPut(allocator, key.type_id) catch @panic("OOM");
        if (!gop.found_existing) {
            gop.value_ptr.* = std.ArrayListUnmanaged(FieldEntry){};
        }
        gop.value_ptr.append(allocator, .{ .field_index = key.field_index, .name_id = name_id }) catch @panic("OOM");
    }

    // Build switch arms (one per type_id with array of field name IDs)
    var switch_arms = std.ArrayListUnmanaged(u8){};

    var type_it = type_groups.iterator();
    while (type_it.next()) |type_entry| {
        const type_id = type_entry.key_ptr.*;
        const fields = type_entry.value_ptr.*;

        // Find max field index to size the array
        var max_index: u32 = 0;
        for (fields.items) |field| {
            if (field.field_index > max_index) max_index = field.field_index;
        }

        // Build array with name_ids at their field_index positions
        var array_elems = std.ArrayListUnmanaged(u8){};
        var i: u32 = 0;
        while (i <= max_index) : (i += 1) {
            if (i > 0) {
                array_elems.appendSlice(allocator, ", ") catch @panic("OOM");
            }
            // Find the name_id for this field_index
            var found: u32 = 0;
            for (fields.items) |field| {
                if (field.field_index == i) {
                    found = field.name_id;
                    break;
                }
            }
            const elem = clr_allocator.allocPrint(allocator, "{d}", .{found}, null);
            array_elems.appendSlice(allocator, elem) catch @panic("OOM");
        }

        const arm = clr_allocator.allocPrint(allocator,
            \\        {d} => &.{{ {s} }},
            \\
        , .{ type_id, array_elems.items }, null);
        switch_arms.appendSlice(allocator, arm) catch @panic("OOM");
    }

    // Build final function with array lookup
    return clr_allocator.allocPrint(allocator,
        \\pub fn getFieldId(type_id: u32, field_index: u32) ?u32 {{
        \\    const fields: []const u32 = switch (type_id) {{
        \\{s}        else => return null,
        \\    }};
        \\    return fields[field_index];
        \\}}
        \\
    , .{switch_arms.items}, null);
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
        \\const Arg = clr.Arg;
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
        \\    ctx.getName = &getName;
        \\    ctx.getFieldId = &getFieldId;
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

    // Build parameter list: ctx + caller_refinements + arity Arg args
    var params: []const u8 = "ctx: *Context, caller_refinements: ?*Refinements";
    var i: u32 = 0;
    while (i < arity) : (i += 1) {
        params = clr_allocator.allocPrint(arena.allocator(), "{s}, _: Arg", .{params}, null);
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
