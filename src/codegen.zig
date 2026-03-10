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
pub const FieldHashMap = clr.FieldHashMap;

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
        .unwrap_errunion_payload, .optional_payload, .optional_payload_ptr, .unwrap_errunion_payload_ptr => payloadTransferOp(info, datum),
        .errunion_payload_ptr_set => payloadErrunionPayloadPtrSet(info, datum),
        .wrap_errunion_payload => payloadWrapErrunionPayload(info, datum),
        .wrap_optional => payloadWrapOptional(info, datum),
        .is_non_null, .is_null, .is_non_err, .is_non_null_ptr, .is_null_ptr => payloadUnOp(info, datum),
        // Additional is_* checks using un_op
        .is_err, .is_err_ptr, .is_non_err_ptr, .error_set_has_value => payloadUnOp(info, datum),
        .br => payloadBr(info, datum),
        .block => payloadBlock(info, datum),
        .struct_field_ptr_index_0 => payloadStructFieldPtr(info, datum, 0),
        .struct_field_ptr_index_1 => payloadStructFieldPtr(info, datum, 1),
        .struct_field_ptr_index_2 => payloadStructFieldPtr(info, datum, 2),
        .struct_field_ptr_index_3 => payloadStructFieldPtr(info, datum, 3),
        // Plain struct_field_ptr uses ty_pl with StructField payload (for field_index > 3)
        .struct_field_ptr => payloadStructFieldPtrGeneric(info, datum),
        .struct_field_val => payloadStructFieldVal(info, datum),
        .field_parent_ptr => payloadFieldParentPtr(info, datum),
        .ret_ptr => payloadRetPtr(info, datum),
        .ret_load => payloadRetLoad(info, datum),
        .set_union_tag => payloadSetUnionTag(info, datum),
        .get_union_tag => payloadGetUnionTag(info, datum),
        .@"try", .try_cold => payloadTry(info, datum),
        // try_ptr and try_ptr_cold use ty_pl like try
        .try_ptr, .try_ptr_cold => payloadTry(info, datum),
        .array_elem_val, .slice_elem_val => payloadArrayElemVal(info, datum),
        // ptr_elem_val uses bin_op like array_elem_val
        .ptr_elem_val => payloadArrayElemVal(info, datum),
        .ptr_elem_ptr, .slice_elem_ptr => payloadPtrElemPtr(info, datum),
        .slice_ptr => payloadSlicePtr(info, datum),
        .array_to_slice => payloadArrayToSlice(info, datum),
        .ptr_add, .ptr_sub => payloadPtrAdd(info, datum),
        // Slice pointer access (ty_op)
        .ptr_slice_len_ptr, .ptr_slice_ptr_ptr => payloadTransferOp(info, datum),
        // Additional ty_op operations that transfer properties
        .optional_payload_ptr_set, .unwrap_errunion_err_ptr => payloadTransferOp(info, datum),
        // Type conversion ops (ty_op)
        .fpext, .fptrunc, .intcast_safe, .addrspace_cast => payloadTransferOp(info, datum),
        .int_from_float, .int_from_float_optimized, .int_from_float_safe, .int_from_float_optimized_safe => payloadTransferOp(info, datum),
        .float_from_int => payloadTransferOp(info, datum),
        // Math unary ops (un_op) - produce scalar result
        .sqrt, .sin, .cos, .tan, .exp, .exp2, .log, .log2, .log10 => payloadUnOp(info, datum),
        .floor, .ceil, .round, .trunc_float, .neg, .neg_optimized, .abs => payloadUnOp(info, datum),
        // Bit operations (ty_op) - these use ty_op but UnOp expects src
        .popcount, .byte_swap, .bit_reverse, .clz => payloadTransferOp(info, datum),
        // Other ty_op operations that produce scalar (not, trunc, ctz, slice_len, etc.)
        .not, .trunc, .ctz, .slice_len, .unwrap_errunion_err => payloadTransferOp(info, datum),
        // is_named_enum_value uses un_op
        .is_named_enum_value => payloadUnOp(info, datum),
        // Vector ops
        .splat => payloadTransferOp(info, datum),
        // C varargs (ty_op)
        .c_va_arg, .c_va_copy => payloadTransferOp(info, datum),
        // Binary arithmetic (bin_op)
        .add, .sub, .mul, .div_trunc, .div_floor, .div_exact, .rem, .mod => payloadBinOp(info, datum),
        .min, .max => payloadBinOp(info, datum),
        .bit_and, .bit_or, .xor => payloadBinOp(info, datum),
        .shl, .shr => payloadBinOp(info, datum),
        .cmp_eq, .cmp_neq, .cmp_lt, .cmp_lte, .cmp_gt, .cmp_gte => payloadBinOp(info, datum),
        .bool_and, .bool_or => payloadBinOp(info, datum),
        // Safe/wrap/sat variants (bin_op)
        .add_safe, .add_wrap, .add_sat, .sub_safe, .sub_wrap, .sub_sat => payloadBinOp(info, datum),
        .mul_safe, .mul_wrap, .mul_sat => payloadBinOp(info, datum),
        .div_float, .shr_exact, .shl_exact, .shl_sat => payloadBinOp(info, datum),
        // Optimized variants (bin_op)
        .add_optimized, .sub_optimized, .mul_optimized => payloadBinOp(info, datum),
        .div_float_optimized, .div_trunc_optimized, .div_floor_optimized, .div_exact_optimized => payloadBinOp(info, datum),
        .rem_optimized, .mod_optimized => payloadBinOp(info, datum),
        .cmp_lt_optimized, .cmp_lte_optimized, .cmp_eq_optimized => payloadBinOp(info, datum),
        .cmp_gte_optimized, .cmp_gt_optimized, .cmp_neq_optimized => payloadBinOp(info, datum),
        .cmp_lt_errors_len => payloadBinOp(info, datum),
        .loop => ".{}", // Loop uses Block payload, but tag handler doesn't need it
        .repeat => payloadRepeat(info, datum),
        .switch_dispatch => payloadSwitchDispatch(info, datum),
        // Void/noreturn tags - no payload needed
        .ret, .trap, .breakpoint, .unreach => ".{}",
        .atomic_store_unordered, .atomic_store_monotonic, .atomic_store_release, .atomic_store_seq_cst => ".{}",
        .memset, .memset_safe, .memmove, .memcpy => ".{}",
        .prefetch, .set_err_return_trace, .save_err_return_trace_index => ".{}",
        .vector_store_elem => ".{}",
        .c_va_end => ".{}",
        // Error trace (ty field only)
        .err_return_trace, .c_va_start => ".{}",
        // Frame/runtime addresses
        .frame_addr, .ret_addr => ".{}",
        // Runtime nav pointer
        .runtime_nav_ptr => ".{}",
        // Inferred allocs
        .inferred_alloc, .inferred_alloc_comptime => ".{}",
        // Atomic ops - complex payloads, stub for now
        .atomic_load, .atomic_rmw => ".{}",
        .cmpxchg_weak, .cmpxchg_strong => ".{}",
        // Shuffle ops (ty_pl)
        .shuffle_one, .shuffle_two => ".{}",
        // Reduce ops
        .reduce, .reduce_optimized => ".{}",
        // WASM ops
        .wasm_memory_grow, .wasm_memory_size => ".{}",
        .work_group_id, .work_group_size, .work_item_id => ".{}",
        // Mul add (pl_op)
        .mul_add => ".{}",
        // Assembly
        .assembly => ".{}",
        // Tag/error name
        .tag_name, .error_name => ".{}",
        // Cmp vector
        .cmp_vector, .cmp_vector_optimized => ".{}",
        // Overflow ops (ty_pl)
        .shl_with_overflow => ".{}",
        .aggregate_init => payloadAggregateInit(info, datum),
        else => ".{}",
    };
}

/// Payload for binary operations using bin_op field
fn payloadBinOp(info: *const FnInfo, datum: Data) []const u8 {
    const lhs_str = srcString(info, datum.bin_op.lhs);
    const rhs_str = srcString(info, datum.bin_op.rhs);
    return clr_allocator.allocPrint(info.arena, ".{{ .lhs = {s}, .rhs = {s} }}", .{ lhs_str, rhs_str }, null);
}

/// Payload for plain struct_field_ptr (field_index > 3, uses ty_pl with StructField)
fn payloadStructFieldPtrGeneric(info: *const FnInfo, datum: Data) []const u8 {
    const ty_str = if (datum.ty_pl.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unimplemented = {} }";

    // Extract from extra data (StructField payload has two u32 fields)
    const payload_index = datum.ty_pl.payload;
    // StructField: struct_operand (Ref as u32), field_index (u32)
    const operand_raw = info.extra[payload_index];
    const field_index = info.extra[payload_index + 1];
    const operand: Ref = @enumFromInt(operand_raw);
    const base_src = srcString(info, operand);

    // Extract container type_id from operand's type
    const type_id: u32 = blk: {
        const operand_type: ?InternPool.Index = if (operand.toIndex()) |idx|
            getInstResultType(info.ip, info.tags, info.data, @intFromEnum(idx))
        else if (operand.toInterned()) |interned_idx|
            info.ip.typeOf(interned_idx)
        else
            null;

        if (operand_type) |ptr_type_idx| {
            const ptr_key = info.ip.indexToKey(ptr_type_idx);
            if (ptr_key == .ptr_type) {
                break :blk @intFromEnum(ptr_key.ptr_type.child);
            }
        }
        break :blk 0;
    };

    return clr_allocator.allocPrint(info.arena, ".{{ .base = {s}, .field_index = {d}, .ty = {s}, .type_id = {d} }}", .{ base_src, field_index, ty_str, type_id }, null);
}

/// Payload for operations that transfer all properties from source to result.
/// Used for unwrap_errunion_payload, optional_payload, etc. where the result carries
/// the same memory safety / allocation state as the source.
fn payloadTransferOp(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for wrap_errunion_payload - wraps a value in an error union (success case).
/// Takes a payload value and creates an error union containing that value.
fn payloadWrapErrunionPayload(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info, operand);
    const ty_str = if (datum.ty_op.ty.toInternedAllowNone()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .errorunion = &.{ .scalar = {} } }";
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s}, .ty = {s} }}", .{ src_str, ty_str }, null);
}

/// Payload for wrap_optional - wraps a value in an optional (non-null case).
/// Takes a payload value and creates an optional containing that value.
fn payloadWrapOptional(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info, operand);
    const ty_str = if (datum.ty_op.ty.toInternedAllowNone()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .optional = &.{ .scalar = {} } }";
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s}, .ty = {s} }}", .{ src_str, ty_str }, null);
}

/// Payload for errunion_payload_ptr_set - gets pointer to error union payload.
/// Takes pointer to error union *(E!T) and returns pointer to payload *T.
fn payloadErrunionPayloadPtrSet(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    // operand is the pointer to the error union
    if (operand.toIndex()) |idx| {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {d} }}", .{@intFromEnum(idx)}, null);
    }
    // Interned/other case - shouldn't happen for pointer operations
    return ".{ .ptr = null }";
}

/// Payload for .try/.try_cold - extracts success payload from error union.
/// Uses pl_op.operand to get the error union source.
fn payloadTry(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.pl_op.operand;
    const src_str = srcString(info, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for bitcast - includes destination type for analysis.
fn payloadBitcast(info: *const FnInfo, datum: Data) []const u8 {
    const operand = datum.ty_op.operand;
    const src_str = srcString(info, operand);
    const ty_str = if (datum.ty_op.ty.toInternedAllowNone()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unimplemented = {} }";
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s}, .ty = {s} }}", .{ src_str, ty_str }, null);
}

/// Payload for un_op instructions (is_non_null, is_null, etc.) that use datum.un_op
fn payloadUnOp(info: *const FnInfo, datum: Data) []const u8 {
    const src_str = srcString(info, datum.un_op);
    return clr_allocator.allocPrint(info.arena, ".{{ .src = {s} }}", .{src_str}, null);
}

/// Payload for array_elem_val - gets a value from an array/region at an index.
/// bin_op: lhs = array/region, rhs = index (ignored for uniform region model)
fn payloadArrayElemVal(info: *const FnInfo, datum: Data) []const u8 {
    // lhs is the array/region source
    const base_str = srcString(info, datum.bin_op.lhs);
    return clr_allocator.allocPrint(info.arena, ".{{ .base = {s} }}", .{base_str}, null);
}

/// Payload for ptr_elem_ptr - gets a pointer to an array/region element.
/// ty_pl with Bin payload: lhs = pointer to array/region, rhs = index (ignored for uniform region model)
fn payloadPtrElemPtr(info: *const FnInfo, datum: Data) []const u8 {
    const payload_index = datum.ty_pl.payload;
    // Bin: lhs (Ref as u32), rhs (Ref as u32)
    const lhs_raw = info.extra[payload_index];
    const lhs_ref: Ref = @enumFromInt(lhs_raw);
    const base_str = srcString(info, lhs_ref);
    return clr_allocator.allocPrint(info.arena, ".{{ .base = {s} }}", .{base_str}, null);
}

/// Payload for slice_ptr - extracts the pointer from a slice.
/// ty_op format: operand is the slice value.
fn payloadSlicePtr(info: *const FnInfo, datum: Data) []const u8 {
    // Read ty_op as raw bytes to get the operand
    const raw_ptr: [*]const u32 = @ptrCast(&datum);
    // ty_op: { ty: Ref (u32), operand: Ref (u32) }
    const operand_raw: u32 = raw_ptr[1];
    const operand_ref: Ref = @enumFromInt(operand_raw);
    const slice: ?usize = if (operand_ref.toIndex()) |idx| @intFromEnum(idx) else null;
    return clr_allocator.allocPrint(info.arena, ".{{ .slice = {?d} }}", .{slice}, null);
}

/// Payload for array_to_slice - converts array/many-pointer to slice.
/// ty_op format: operand is the array/many-pointer value.
fn payloadArrayToSlice(info: *const FnInfo, datum: Data) []const u8 {
    // Read ty_op as raw bytes to get the operand
    const raw_ptr: [*]const u32 = @ptrCast(&datum);
    // ty_op: { ty: Ref (u32), operand: Ref (u32) }
    const operand_raw: u32 = raw_ptr[1];
    const operand_ref: Ref = @enumFromInt(operand_raw);
    const source_str = srcString(info, operand_ref);
    return clr_allocator.allocPrint(info.arena, ".{{ .source = {s} }}", .{source_str}, null);
}

/// Payload for ptr_add/ptr_sub - pointer arithmetic.
/// ty_pl with Bin payload: lhs = pointer, rhs = offset.
fn payloadPtrAdd(info: *const FnInfo, datum: Data) []const u8 {
    const payload_index = datum.ty_pl.payload;
    // Bin: lhs (Ref as u32), rhs (Ref as u32)
    const lhs_raw = info.extra[payload_index];
    const lhs_ref: Ref = @enumFromInt(lhs_raw);
    const ptr_str = srcString(info, lhs_ref);
    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {s} }}", .{ptr_str}, null);
}

/// Payload for ret_safe - just the src, caller_refinements and return_gid come from State.
fn payloadRetSafe(info: *const FnInfo, datum: Data) []const u8 {
    const src_str = srcString(info, datum.un_op);
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

/// Payload for aggregate_init (creates struct/array/tuple)
fn payloadAggregateInit(info: *const FnInfo, datum: Data) []const u8 {
    const result_type = datum.ty_pl.ty.toInterned() orelse return ".{ .ty = .{ .unimplemented = {} }, .elements = &.{} }";
    const type_str = typeToString(info.name_map, info.field_map, info.arena, info.ip, result_type);

    // Get element count from the type
    const type_key = info.ip.indexToKey(result_type);
    const elem_count: usize = switch (type_key) {
        .struct_type => blk: {
            const loaded = info.ip.loadStructType(result_type);
            break :blk loaded.field_types.get(info.ip).len;
        },
        .tuple_type => |t| t.types.len,
        .array_type => |a| @intCast(a.len),
        .vector_type => |v| v.len,
        else => 0,
    };

    if (elem_count == 0) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s}, .elements = &.{{}} }}", .{type_str}, null);
    }

    // Extract payload index - use raw pointer access for DLL safety
    const raw_ptr: [*]const u32 = @ptrCast(&datum);
    // ty_pl: { ty: Ref (u32), payload: u32 }
    const payload_index: usize = @intCast(raw_ptr[1]);

    // Build elements - fully inlined to avoid DLL issues
    if (elem_count == 0) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s}, .elements = &.{{}} }}", .{type_str}, null);
    }

    // Get first element
    const elem0: []const u8 = if (payload_index < info.extra.len) blk: {
        const raw = info.extra[payload_index];
        const ref: Ref = @enumFromInt(raw);
        break :blk srcString(info, ref);
    } else ".{ .inst = 0 }";

    if (elem_count == 1) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s}, .elements = &.{{ {s} }} }}", .{ type_str, elem0 }, null);
    }

    // Get second element
    const elem1: []const u8 = if (payload_index + 1 < info.extra.len) blk: {
        const raw = info.extra[payload_index + 1];
        const ref: Ref = @enumFromInt(raw);
        break :blk srcString(info, ref);
    } else ".{ .inst = 0 }";

    if (elem_count == 2) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s}, .elements = &.{{ {s}, {s} }} }}", .{ type_str, elem0, elem1 }, null);
    }

    // For more than 2 elements, just return unimplemented for now
    return clr_allocator.allocPrint(info.arena, ".{{ .ty = {s}, .elements = &.{{}} }}", .{type_str}, null);
}

/// Extract the return type string for a function given its InternPool index.
/// Returns the type as a Type union initializer string (e.g., ".{ .scalar = {} }").
/// Used to initialize return slots with proper type structure.
/// Special case: if return type is std.mem.Allocator, emits allocator type with type_id from FQN.
pub fn extractFunctionReturnType(
    name_map: *std.AutoHashMapUnmanaged(u32, []const u8),
    field_map: *FieldHashMap,
    arena: std.mem.Allocator,
    ip: *const InternPool,
    func_ip_idx: InternPool.Index,
) []const u8 {
    // Get function and its type
    const func_key = ip.indexToKey(func_ip_idx);
    const func_ty = switch (func_key) {
        .func => |f| f.ty,
        else => return ".{ .unimplemented = {} }", // unhandled function type
    };

    // Get function type to access return_type
    const func_type_key = ip.indexToKey(func_ty);
    const return_type_idx = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return ".{ .unimplemented = {} }", // unhandled function type
    };

    // Special case: if return type is std.mem.Allocator, emit allocator type with type_id
    // This handles the MkAllocator case - calls like gpa.allocator() that return Allocator
    if (isAllocatorType(ip, return_type_idx)) {
        // Get callee's FQN to compute type_id
        const nav = switch (func_key) {
            .func => |f| ip.getNav(f.owner_nav),
            else => return ".{ .allocator = 0 }", // fallback
        };
        const fqn = nav.fqn.toSlice(ip);
        const type_id = fqnTypeId(fqn);
        // Register name for error messages
        const type_name = extractTypeFromAllocatorMethod(fqn);
        if (name_map.get(type_id) == null) {
            name_map.put(arena, type_id, type_name) catch {};
        }
        return clr_allocator.allocPrint(arena, ".{{ .allocator = {d} }}", .{type_id}, null);
    }

    // Convert return type to string
    return typeToString(name_map, field_map, arena, ip, return_type_idx);
}

/// Extract the actual function InternPool index from a function pointer constant.
/// When we have `const fp = &myFunction`, the interned value is a pointer to the function.
/// This extracts the underlying function's IP index if possible.
/// Returns null if the value is not a function pointer constant.
fn extractFunctionFromPointer(ip: *const InternPool, ip_idx: InternPool.Index) ?InternPool.Index {
    const val_key = ip.indexToKey(ip_idx);

    // Check if it's already a function (not a pointer)
    switch (val_key) {
        .func => return ip_idx, // Already a function, return as-is
        .ptr => |ptr| {
            // It's a pointer - check if it points to a function via nav
            switch (ptr.base_addr) {
                .nav => |nav_idx| {
                    // Get the nav to access its function value if fully resolved
                    const nav = ip.getNav(nav_idx);
                    // Only fully_resolved status has the actual value
                    const func_index = switch (nav.status) {
                        .fully_resolved => |r| r.val,
                        else => return null,
                    };
                    // Verify the resolved value is actually a function
                    const func_key = ip.indexToKey(func_index);
                    if (func_key == .func) {
                        return func_index;
                    }
                    return null;
                },
                else => return null,
            }
        },
        else => return null,
    }
}

/// Extract type info for AllocCreate payload.
/// allocator.create(T) returns Allocator.Error!*T, so we unwrap error union then pointer.
/// NOTE: Returns ".{ .unimplemented = {} }" when type cannot be determined.
/// This will crash at runtime if the refinement is accessed, surfacing extraction failures.
fn extractAllocCreateType(info: *const FnInfo, datum: Data) []const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return ".{ .unimplemented = {} }";

    // Get function and its type
    const func_key = info.ip.indexToKey(ip_idx);
    const func_ty = switch (func_key) {
        .func => |f| f.ty,
        else => return ".{ .unimplemented = {} }",
    };

    // Get function type to access return_type
    const func_type_key = info.ip.indexToKey(func_ty);
    const return_type = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return ".{ .unimplemented = {} }",
    };

    // Return type is Allocator.Error!*T - unwrap error union to get *T
    const ptr_type: InternPool.Index = blk: {
        const return_key = info.ip.indexToKey(return_type);
        break :blk switch (return_key) {
            .error_union_type => |eu| eu.payload_type,
            .ptr_type => return_type, // Already unwrapped
            else => return ".{ .unimplemented = {} }",
        };
    };

    // Now unwrap *T to get T
    const pointee_type: InternPool.Index = blk: {
        const ptr_key = info.ip.indexToKey(ptr_type);
        break :blk switch (ptr_key) {
            .ptr_type => |p| p.child,
            else => return ".{ .unimplemented = {} }",
        };
    };

    return typeToString(info.name_map, info.field_map, info.arena, info.ip, pointee_type);
}

/// Extract element type T from allocator.alloc() return type Allocator.Error![]T
fn extractAllocAllocType(info: *const FnInfo, datum: Data) []const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return ".{ .unimplemented = {} }";

    // Get function and its type
    const func_key = info.ip.indexToKey(ip_idx);
    const func_ty = switch (func_key) {
        .func => |f| f.ty,
        else => return ".{ .unimplemented = {} }",
    };

    // Get function type to access return_type
    const func_type_key = info.ip.indexToKey(func_ty);
    const return_type = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return ".{ .unimplemented = {} }",
    };

    // Return type is Allocator.Error![]T or ?[]T - unwrap to get []T
    const slice_type: InternPool.Index = blk: {
        const return_key = info.ip.indexToKey(return_type);
        break :blk switch (return_key) {
            .error_union_type => |eu| eu.payload_type,
            .opt_type => |child| child, // remap returns ?[]T
            .ptr_type => return_type, // Already unwrapped
            else => return ".{ .unimplemented = {} }",
        };
    };

    // Now unwrap []T to get element type T
    // Slice is ptr_type with .flags.size = .slice
    const element_type: InternPool.Index = blk: {
        const slice_key = info.ip.indexToKey(slice_type);
        break :blk switch (slice_key) {
            .ptr_type => |p| p.child,
            else => return ".{ .unimplemented = {} }",
        };
    };

    return typeToString(info.name_map, info.field_map, info.arena, info.ip, element_type);
}

/// Payload for struct_field_ptr_index_N - gets pointer to a struct field.
/// Field names are looked up at runtime via ctx.getFieldId(type_id, field_index).
fn payloadStructFieldPtr(info: *const FnInfo, datum: Data, field_index: usize) []const u8 {
    // ty_op.ty is the result type (pointer to field), ty_op.operand is the base struct pointer
    const ty_str = if (datum.ty_op.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unimplemented = {} }"; // Will cause compile error if hit
    const base_src = srcString(info, datum.ty_op.operand);

    // Extract container type_id from operand's type (pointer to struct -> struct type)
    const type_id: u32 = blk: {
        // Try to get operand type from instruction or interned
        const operand_type: ?InternPool.Index = if (datum.ty_op.operand.toIndex()) |idx|
            getInstResultType(info.ip, info.tags, info.data, @intFromEnum(idx))
        else if (datum.ty_op.operand.toInterned()) |interned_idx|
            info.ip.typeOf(interned_idx) // typeOf returns InternPool.Index directly
        else
            null;

        if (operand_type) |ptr_type_idx| {
            const ptr_key = info.ip.indexToKey(ptr_type_idx);
            if (ptr_key == .ptr_type) {
                break :blk @intFromEnum(ptr_key.ptr_type.child);
            }
        }
        break :blk 0;
    };

    return clr_allocator.allocPrint(info.arena, ".{{ .base = {s}, .field_index = {d}, .ty = {s}, .type_id = {d} }}", .{ base_src, field_index, ty_str, type_id }, null);
}

/// Payload for struct_field_val - extracts a field value from a struct by value.
/// Uses ty_pl with StructField payload: struct_operand and field_index.
/// Field names are looked up at runtime via ctx.getFieldId(type_id, field_index).
fn payloadStructFieldVal(info: *const FnInfo, datum: Data) []const u8 {
    // ty_pl has ty (Ref) and payload index (u32)
    const ty_str = if (datum.ty_pl.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unimplemented = {} }"; // Will cause compile error if hit
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
        ".{ .unimplemented = {} }";
    const payload_index = datum.ty_pl.payload;

    // FieldParentPtr: field_ptr (Ref as u32), field_index (u32)
    const field_ptr_raw = info.extra[payload_index];
    const field_index = info.extra[payload_index + 1];

    // Convert the raw u32 to a Ref and use srcString
    const field_ptr_ref: Ref = @enumFromInt(field_ptr_raw);
    const field_ptr_str = srcString(info, field_ptr_ref);

    // Extract container type_id from pointer type (pointer -> container)
    const type_id: u32 = if (datum.ty_pl.ty.toInterned()) |ty_idx| blk: {
        const ptr_type_key = info.ip.indexToKey(ty_idx);
        if (ptr_type_key == .ptr_type) {
            break :blk @intFromEnum(ptr_type_key.ptr_type.child);
        }
        break :blk 0;
    } else 0;

    return clr_allocator.allocPrint(info.arena, ".{{ .field_ptr = {s}, .field_index = {d}, .ty = {s}, .type_id = {d} }}", .{ field_ptr_str, field_index, ty_str, type_id }, null);
}

/// Payload for br (branch to block with value).
/// Transfers properties from operand to target block.
fn payloadBr(info: *const FnInfo, datum: Data) []const u8 {
    const block_inst = @intFromEnum(datum.br.block_inst);
    const operand = datum.br.operand;
    const src_str = srcString(info, operand);
    return clr_allocator.allocPrint(info.arena, ".{{ .block = {d}, .src = {s} }}", .{ block_inst, src_str }, null);
}

/// Payload for repeat - extracts the loop instruction index.
fn payloadRepeat(info: *const FnInfo, datum: Data) []const u8 {
    const loop_inst = @intFromEnum(datum.repeat.loop_inst);
    return clr_allocator.allocPrint(info.arena, ".{{ .loop_inst = {d} }}", .{loop_inst}, null);
}

/// Payload for switch_dispatch - dispatches to another case of a loop_switch_br.
/// Uses the br field: block_inst is the loop_switch_br instruction, operand is target case value.
fn payloadSwitchDispatch(info: *const FnInfo, datum: Data) []const u8 {
    const loop_switch_inst: u32 = @intFromEnum(datum.br.block_inst);
    const operand = datum.br.operand;

    // Find target case by matching operand against case items
    var target_case: u32 = 0; // Default to case 0 if not found
    if (extractSwitchBrBodies(info.arena, info.ip, loop_switch_inst, info.tags, info.data, info.extra)) |bodies| {
        // Match operand against each case's first_item
        for (bodies.cases, 0..) |case, i| {
            if (case.first_item) |item| {
                if (@intFromEnum(item) == @intFromEnum(operand)) {
                    target_case = @intCast(i);
                    break;
                }
            }
        }
    }

    return clr_allocator.allocPrint(info.arena, ".{{ .loop_switch_inst = {d}, .target_case = {d} }}", .{ loop_switch_inst, target_case }, null);
}

/// Payload for block - extracts the block's result type.
fn payloadBlock(info: *const FnInfo, datum: Data) []const u8 {
    const ty_ref = datum.ty_pl.ty;

    // Check for well-known types that don't require InternPool lookup
    if (ty_ref == .none) {
        return ".{ .ty = .{ .void = {} } }";
    }
    if (ty_ref == .void_type) {
        return ".{ .ty = .{ .void = {} } }";
    }
    if (ty_ref == .noreturn_type) {
        return ".{ .ty = .{ .void = {} } }";
    }

    // Try to get interned index
    const ty_idx = ty_ref.toInternedAllowNone() orelse return ".{ .ty = .{ .void = {} } }";

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
                break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .noop_debug = .{{}} }});\n", .{inst_index}, null);
            }
            // All allocator operations handled by runtime call filter in memory_safety.call():
            // - AllocCreate, AllocDestroy, AllocAlloc, AllocFree
            // - alignedAlloc, resize, realloc, remap
            // - dupe, dupeZ
            // - ArenaAllocator.init, ArenaAllocator.deinit
            // MkAllocator is handled by extractFunctionReturnType emitting .allocator type
            // when the return type is std.mem.Allocator. Arena linking is done in memory_safety.call().
            const call_parts = payloadCallParts(info, datum);
            const fqn = getCallFqn(info.ip, datum) orelse "";
            break :blk clr_allocator.allocPrint(info.arena, "    try Inst.call(state, {d}, {s}, {s}, {s}, \"{s}\");\n", .{ inst_index, call_parts.called, call_parts.return_type, call_parts.args, fqn }, null);
        },
        .store, .store_safe => blk: {
            // Check if storing a function pointer constant
            // Only check for fnptr if the source is an interned value (not a well-known type)
            if (datum.bin_op.rhs.toInterned()) |interned_idx| {
                // Skip well-known types like .undef, .void_type etc. which don't have valid IP entries in tests
                if (!isWellKnownIndex(interned_idx)) {
                    const ty = info.ip.typeOf(interned_idx);
                    if (!isWellKnownType(ty)) {
                        const type_key = info.ip.indexToKey(ty);
                        if (type_key == .ptr_type and info.ip.indexToKey(type_key.ptr_type.child) == .func_type) {
                            // Function pointer constant - use storeFnptr
                            if (extractFunctionFromPointer(info.ip, interned_idx)) |func_idx| {
                                // Get ptr string
                                const ptr_str: []const u8 = if (datum.bin_op.lhs.toIndex()) |idx|
                                    clr_allocator.allocPrint(info.arena, ".{{ .inst = {d} }}", .{@intFromEnum(idx)}, null)
                                else if (datum.bin_op.lhs.toInterned()) |ptr_interned_idx| blk2: {
                                    // Register globals (side effect)
                                    _ = tryFieldPtrRef(info, ptr_interned_idx);
                                    _ = tryGlobalRef(info, ptr_interned_idx);
                                    const ptr_ip_idx: u32 = @intFromEnum(ptr_interned_idx);
                                    const ptr_ty = info.ip.typeOf(ptr_interned_idx);
                                    const ptr_ty_str = typeToString(null, null, info.arena, info.ip, ptr_ty);
                                    break :blk2 clr_allocator.allocPrint(info.arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = {s} }} }}", .{ ptr_ip_idx, ptr_ty_str }, null);
                                } else ".{ .interned = .{ .ip_idx = 0, .ty = .{ .unimplemented = {} } } }";
                                break :blk clr_allocator.allocPrint(info.arena, "    try Inst.storeFnptr(state, {d}, {s}, fn_{d});\n", .{ inst_index, ptr_str, @intFromEnum(func_idx) }, null);
                            }
                        }
                    }
                }
            }
            // Regular store - use standard apply
            const tag_payload = payload(info, tag, datum, arg_counter);
            break :blk clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .{s} = {s} }});\n", .{ inst_index, altName(tag), tag_payload }, null);
        },
        else => blk: {
            const tag_payload = payload(info, tag, datum, arg_counter);
            const result = clr_allocator.allocPrint(info.arena, "    try Inst.apply(state, {d}, .{{ .{s} = {s} }});\n", .{ inst_index, altName(tag), tag_payload }, null);
            break :blk result;
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
    // Use args[N] to access the argument from the unified args slice
    return clr_allocator.allocPrint(info.arena, ".{{ .value = args[{d}], .name_id = {d} }}", .{ arg_index, name_id }, null);
}

fn payloadDbgStmt(info: *const FnInfo, datum: Data) []const u8 {
    return clr_allocator.allocPrint(info.arena, ".{{ .line = {d}, .column = {d} }}", .{
        datum.dbg_stmt.line,
        datum.dbg_stmt.column,
    }, null);
}

/// Convert an AIR Ref to a Src union string.
/// Handles .inst (runtime instruction), .interned (globals and comptime), and .fnptr (function pointers).
fn srcString(info: *const FnInfo, ref: Ref) []const u8 {
    const arena = info.arena;
    const ip = info.ip;
    // Check for .none first (void/no value)
    if (ref == .none) {
        return ".{ .interned = .{ .ip_idx = 0, .ty = .{ .void = {} } } }";
    }
    if (ref.toIndex()) |idx| {
        // Runtime value from instruction
        return clr_allocator.allocPrint(arena, ".{{ .inst = {d} }}", .{@intFromEnum(idx)}, null);
    }
    if (ref.toInterned()) |interned_idx| {
        const ip_idx: u32 = @intFromEnum(interned_idx);

        // Check for null value first (untyped null)
        if (interned_idx == .null_value) {
            // Untyped null - we don't know the optional type
            return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = .{{ .unimplemented = {{}} }} }} }}", .{ip_idx}, null);
        }

        // Try to register as global (side effect: adds to global_map if applicable)
        // We call these for their side effects, not their return values
        _ = tryFieldPtrRef(info, interned_idx);
        _ = tryGlobalRef(info, interned_idx);

        // Determine type string for this interned value
        const ty = ip.typeOf(interned_idx);
        const type_str = blk: {
            if (!isWellKnownType(ty)) {
                const type_key = ip.indexToKey(ty);

                // Check for typed null (optional with null value)
                if (type_key == .opt_type) {
                    const val_key = ip.indexToKey(interned_idx);
                    if (val_key == .opt and val_key.opt.val == .none) {
                        const child_str = typeToString(null, null, arena, ip, type_key.opt_type);
                        break :blk clr_allocator.allocPrint(arena, ".{{ .null = &{s} }}", .{child_str}, null);
                    }
                }

                // Check for Allocator struct value - emit .allocator type with type_id
                if (isAllocatorType(ip, ty)) {
                    const alloc_info = extractAllocatorTypeFromInterned(ip, interned_idx);
                    // Register name for error messages if we have name_map access
                    if (info.name_map.get(alloc_info.id) == null) {
                        info.name_map.put(arena, alloc_info.id, alloc_info.name) catch {};
                    }
                    // Register allocator for identity tracking (mismatch detection)
                    // Use global allocator since registerGlobal stores the string
                    const alloc_type_str = clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .allocator = {d} }}", .{alloc_info.id}, null);
                    _ = clr.registerGlobal(@intFromEnum(interned_idx), alloc_type_str, null, .{ .scalar = {} });
                    break :blk alloc_type_str;
                }

                // Check for aggregate value (struct) with field-level undefined
                if (type_key == .struct_type) {
                    const val_key = ip.indexToKey(interned_idx);
                    if (val_key == .aggregate) {
                        const agg = val_key.aggregate;
                        break :blk aggregateValueToString(arena, ip, ty, agg);
                    }
                }

                // Function pointer constant - use fnptr variant (separate from interned)
                if (type_key == .ptr_type and ip.indexToKey(type_key.ptr_type.child) == .func_type) {
                    if (extractFunctionFromPointer(ip, interned_idx)) |func_idx| {
                        // Add to call targets so the function is included in tree shaking
                        const func_type = ip.indexToKey(func_idx).func;
                        const func_type_key = ip.indexToKey(func_type.ty);
                        const arity: u32 = if (func_type_key == .func_type)
                            @intCast(func_type_key.func_type.param_types.len)
                        else
                            0;
                        info.call_targets.append(clr_allocator.allocator(), .{
                            .index = @intFromEnum(func_idx),
                            .arity = arity,
                        }) catch {};
                        return clr_allocator.allocPrint(arena, ".{{ .fnptr = &.{{ &fn_{d} }} }}", .{@intFromEnum(func_idx)}, null);
                    }
                    // Couldn't extract function - return empty fnptr
                    return ".{ .fnptr = &.{} }";
                }
            }
            // Default: use type string
            break :blk typeToString(null, null, arena, ip, ty);
        };

        return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = {s} }} }}", .{ ip_idx, type_str }, null);
    }
    // Fallback - shouldn't normally happen
    return ".{ .interned = .{ .ip_idx = 0, .ty = .{ .unimplemented = {} } } }";
}

/// Check if an interned value is a pointer to a user global variable.
/// If so, register it and return the source string; otherwise return null.
fn tryGlobalRef(info: *const FnInfo, interned_idx: InternPool.Index) ?[]const u8 {
    const ip = info.ip;
    const val_key = ip.indexToKey(interned_idx);

    // The value can be stored as .ptr (with nav metadata) or as .int (raw address)
    // For .int values, we can't determine the nav - these are comptime-computed addresses
    // that will be handled as int_const (and marked as global memory at runtime)
    const ptr: InternPool.Key.Ptr = switch (val_key) {
        .ptr => |p| p,
        else => return null,
    };

    // Check if the pointer has a nav base address (i.e., it's a global variable)
    if (ptr.base_addr != .nav) return null;
    const nav_idx = ptr.base_addr.nav;

    // Get the nav to check if it's a user global
    const nav = ip.getNav(nav_idx);
    const fqn = nav.fqn.toSlice(ip);

    // Check if pointee is an allocator type - these need identity tracking for mismatch detection
    const nav_type = nav.typeOf(ip);
    const is_allocator = isAllocatorType(ip, nav_type);

    // Only track user globals (starts with root module name), unless it's an allocator
    if (!is_allocator and !std.mem.startsWith(u8, fqn, info.root_name)) return null;

    // Check if it's a mutable variable (not a constant)
    // Constants are always defined, so we only track mutable vars - unless it's an allocator
    const is_const = switch (nav.status) {
        .fully_resolved => |fr| fr.is_const,
        .type_resolved => |tr| tr.is_const,
        .unresolved => return null,
    };
    if (!is_allocator and is_const) return null; // Skip constants (but not allocators)

    // Skip field pointers - these should be handled by tryFieldPtrRef
    // A field pointer has non-zero byte_offset OR points to a different type than the nav's type
    if (ptr.byte_offset != 0) return null;
    const ptr_type_key = ip.indexToKey(ptr.ty);
    if (ptr_type_key == .ptr_type) {
        const pointee_type = ptr_type_key.ptr_type.child;
        if (pointee_type != nav_type) return null; // Field pointer, not struct pointer
    }

    // Get the initial value to check if it's undefined
    const init_val = switch (nav.status) {
        .fully_resolved => |fr| fr.val,
        else => return null, // Need fully resolved to check init
    };

    // Check if global is undefined
    // Strategy: Use InternPool's isUndef and isVariable helpers, then access
    // the variable.init field through a volatile read to prevent optimization issues
    const is_undefined = if (init_val != .none) blk: {
        // Direct undef check
        if (ip.isUndef(init_val)) break :blk true;

        // For variables, we need to check their init field
        if (ip.isVariable(init_val)) {
            // Get the Key - copy to local to avoid DLL issues with union field access
            const init_key = ip.indexToKey(init_val);
            // Use volatile pointer to prevent compiler from optimizing the read
            const key_ptr: *const volatile InternPool.Key = @ptrCast(&init_key);
            const stable_key = key_ptr.*;

            // Now safely access the variable field
            const var_init = stable_key.variable.init;
            if (var_init == .none) break :blk true;
            // Use ip.isUndef on the init value
            break :blk ip.isUndef(var_init);
        }
        break :blk false;
    } else true;

    // Check if global is a null-initialized optional
    const is_null = if (!is_undefined and init_val != .none) blk: {
        // Get the actual init value (for variables, look inside)
        const actual_init = if (ip.isVariable(init_val)) actual: {
            const init_key = ip.indexToKey(init_val);
            const key_ptr: *const volatile InternPool.Key = @ptrCast(&init_key);
            const stable_key = key_ptr.*;
            break :actual stable_key.variable.init;
        } else init_val;

        if (actual_init == .none) break :blk false;

        // Check if it's an optional with null value
        const actual_key = ip.indexToKey(actual_init);
        const actual_key_ptr: *const volatile InternPool.Key = @ptrCast(&actual_key);
        const stable_actual = actual_key_ptr.*;
        break :blk switch (stable_actual) {
            .opt => |opt| opt.val == .none,
            else => false,
        };
    } else false;

    // Check if the initial value is a pointer to another global
    // This establishes pointer relationships (e.g., `var ptr: *u8 = &other_global;`)
    // Returns the IP index of the target pointer (not the nav index)
    const target_ip_idx: ?u32 = if (!is_undefined and init_val != .none) blk: {
        // Helper to check if an index is a pointer to a nav-based global
        // Returns the IP index of the pointer itself (the idx parameter)
        const checkPointerToNav = struct {
            fn check(intern_pool: *const InternPool, idx: InternPool.Index) ?u32 {
                if (idx == .none) return null;
                const key = intern_pool.indexToKey(idx);
                // Use volatile to prevent DLL optimization issues
                const key_ptr: *const volatile InternPool.Key = @ptrCast(&key);
                const stable_key = key_ptr.*;
                return switch (stable_key) {
                    .ptr => |p| switch (p.base_addr) {
                        // Return the IP index of this pointer (not the nav index)
                        .nav => @intFromEnum(idx),
                        else => null,
                    },
                    else => null,
                };
            }
        }.check;

        // Try direct check first
        if (checkPointerToNav(ip, init_val)) |target| {
            break :blk target;
        }

        // If init_val is a variable, check its init field
        if (ip.isVariable(init_val)) {
            const init_key = ip.indexToKey(init_val);
            const key_ptr: *const volatile InternPool.Key = @ptrCast(&init_key);
            const stable_key = key_ptr.*;
            const var_init = stable_key.variable.init;
            if (checkPointerToNav(ip, var_init)) |target| {
                break :blk target;
            }
        }

        break :blk null;
    } else null;

    // Get the pointee type (what the pointer points to)
    const ptr_type = ip.indexToKey(ptr.ty);
    const pointee_type = if (ptr_type == .ptr_type) ptr_type.ptr_type.child else .none;
    const inner_type_str = if (pointee_type != .none)
        typeToString(info.name_map, info.field_map, info.arena, ip, pointee_type)
    else
        ".{ .unimplemented = {} }";

    // Wrap in .undefined or .null if global is undefined/null (consistent with store_safe approach)
    // Use global allocator (not arena) because this string must persist until epilogue runs
    const type_str = if (is_undefined)
        clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .undefined = &{s} }}", .{inner_type_str}, null)
    else if (is_null) blk: {
        // For null optionals, .null wraps the INNER type (payload), not the optional itself
        // Extract the optional's child type to avoid double-optional structure
        const opt_child_type = if (pointee_type != .none) opt_blk: {
            const pointee_key = ip.indexToKey(pointee_type);
            // opt_type IS the child type (not a struct with .child field)
            break :opt_blk if (pointee_key == .opt_type) pointee_key.opt_type else pointee_type;
        } else pointee_type;
        const opt_inner_str = if (opt_child_type != .none)
            typeToString(info.name_map, info.field_map, info.arena, ip, opt_child_type)
        else
            ".{ .unimplemented = {} }";
        break :blk clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .null = &{s} }}", .{opt_inner_str}, null);
    } else clr_allocator.allocator().dupe(u8, inner_type_str) catch inner_type_str;

    // Determine children based on target_ip_idx and is_null
    // If this pointer points to another global, set up indirect relationship
    // If it's a null optional, mark it as .@"null"
    // For struct globals, struct_fields will be populated when field pointers are registered
    const children: clr.ChildInfo = if (target_ip_idx) |target|
        .{ .indirect = target }
    else if (is_null)
        .{ .null = {} }
    else
        .{ .scalar = {} };

    // Extract type_id for struct/union pointees (used for field name lookup)
    const type_id: ?u32 = if (pointee_type != .none) blk: {
        const pointee_key = ip.indexToKey(pointee_type);
        if (pointee_key == .struct_type or pointee_key == .union_type) {
            break :blk @intFromEnum(pointee_type);
        }
        break :blk null;
    } else null;

    // Register this global using IP index (the interned pointer value)
    // Also register nav→ip mapping for struct field pointer lookups
    const ip_idx: u32 = @intFromEnum(interned_idx);
    const nav_idx_raw: u32 = @intFromEnum(nav_idx);
    _ = clr.registerGlobalWithNav(ip_idx, nav_idx_raw, type_str, type_id, children);

    // Return just the IP index number as a string
    // Using global allocator instead of arena to avoid potential DLL boundary issues
    return clr_allocator.allocPrint(clr_allocator.allocator(), "{d}", .{ip_idx}, null);
}

/// Check if an interned value is a pointer to a field of a user global struct.
/// This handles comptime-computed field addresses like `&global_struct.field`.
/// Returns the IP index string for int_var if it's a field pointer; otherwise returns null.
fn tryFieldPtrRef(info: *const FnInfo, interned_idx: InternPool.Index) ?[]const u8 {
    const ip = info.ip;
    // Use volatile read to prevent DLL optimization issues with extern union access
    const val_key = ip.indexToKey(interned_idx);

    // Check if this is a pointer type
    switch (val_key) {
        .ptr => {},
        else => return null,
    }

    const ptr = val_key.ptr;

    // For field pointers, base_addr is .field which contains the base struct pointer
    // We need to follow this chain to find the nav
    var nav_idx: InternPool.Nav.Index = undefined;
    var field_index: u32 = undefined;

    // For field pointers, we need to track both the parent struct pointer AND the field pointer
    var parent_ptr_ip_idx: ?InternPool.Index = null;

    switch (ptr.base_addr) {
        .field => |field_info| {
            // This is a field pointer - base is a pointer to the struct
            field_index = @intCast(field_info.index);
            // The base is the parent struct pointer - remember it for registration
            parent_ptr_ip_idx = field_info.base;

            // Get the base pointer (pointer to the struct)
            const base_ptr_key = ip.indexToKey(field_info.base);
            switch (base_ptr_key) {
                .ptr => {
                    // The base pointer should have nav as its base_addr
                    switch (base_ptr_key.ptr.base_addr) {
                        .nav => |nav| {
                            nav_idx = nav;
                        },
                        else => return null, // Base isn't a nav-based pointer
                    }
                },
                else => return null, // Base isn't a pointer
            }
        },
        .nav => {
            // Direct nav pointer with non-zero byte offset could also be a field pointer
            // But for first field (offset 0), we need to check type difference
            nav_idx = ptr.base_addr.nav;
            if (ptr.byte_offset == 0) {
                // Could be &global (not field) - need to check types
                const nav = ip.getNav(nav_idx);
                const container_type = nav.typeOf(ip);
                const pointer_type = ip.typeOf(interned_idx);
                const pointer_key = ip.indexToKey(pointer_type);
                if (pointer_key == .ptr_type) {
                    const pointee_type = pointer_key.ptr_type.child;
                    if (pointee_type == container_type) {
                        // This is &global, not &global.field - not a field pointer
                        return null;
                    }
                }
            }
            // For nav-based pointers with byte_offset, find field by offset
            const nav = ip.getNav(nav_idx);
            const container_type = nav.typeOf(ip);
            const container_key = ip.indexToKey(container_type);
            if (container_key != .struct_type) return null;
            const struct_type = ip.loadStructType(container_type);
            const offsets = struct_type.offsets.get(ip);
            var found_field: ?u32 = null;
            for (offsets, 0..) |offset, i| {
                if (offset == ptr.byte_offset) {
                    found_field = @intCast(i);
                    break;
                }
            }
            field_index = found_field orelse return null;
        },
        else => return null, // Not a nav-based or field-based pointer
    }

    // Get the nav to check if it's a user global
    const nav = ip.getNav(nav_idx);
    const fqn = nav.fqn.toSlice(ip);

    // Only track user globals (starts with root module name)
    if (!std.mem.startsWith(u8, fqn, info.root_name)) return null;

    // Get the type of the container (the global's type)
    const container_type = nav.typeOf(ip);
    const container_key = ip.indexToKey(container_type);

    // Must be a struct type
    if (container_key != .struct_type) return null;

    // Load the struct type to get field types
    const struct_type = ip.loadStructType(container_type);
    const field_types = struct_type.field_types.get(ip);
    const num_fields = field_types.len;

    // Ensure parent struct pointer is registered first (needed for struct_fields linkage)
    // This is necessary when &global.field is used without ever referencing &global directly
    if (parent_ptr_ip_idx) |parent_ip| {
        const parent_ip_idx_raw: u32 = @intFromEnum(parent_ip);
        // GlobalDef.ty is the POINTEE type, not pointer type
        // For struct globals, this is the struct type (with .id for type_id)
        // Use global allocator since this string persists until epilogue
        const parent_type_str = typeToString(info.name_map, info.field_map, clr_allocator.allocator(), ip, container_type);
        const children: clr.ChildInfo = .{ .scalar = {} }; // Will be updated to struct_fields by registerFieldPointer
        const parent_type_id: u32 = @intFromEnum(container_type);
        _ = clr.registerGlobalWithNav(parent_ip_idx_raw, @intFromEnum(nav_idx), parent_type_str, parent_type_id, children);
    }

    // Get the field type for the Src
    const field_type = field_types[field_index];
    // GlobalDef.ty is the POINTEE type - for field pointers, this is the field's type (e.g., i32)
    // Use global allocator since this string persists until epilogue
    const field_type_str = typeToString(info.name_map, info.field_map, clr_allocator.allocator(), ip, field_type);
    const ip_idx: u32 = @intFromEnum(interned_idx);
    const nav_idx_raw: u32 = @intFromEnum(nav_idx);
    clr.registerFieldPointer(nav_idx_raw, field_index, num_fields, ip_idx, field_type_str);

    // Return int_var with IP index (field pointers are now regular int_var entries)
    return clr_allocator.allocPrint(clr_allocator.allocator(), "{d}", .{ip_idx}, null);
}

/// Convert an AIR Ref to a Src union string with .undefined wrapper.
/// Used when storing undefined values - wraps the type in .undefined.
fn srcStringUndef(arena: std.mem.Allocator, ip: *const InternPool, ref: Ref) []const u8 {
    // For undefined stores, we wrap the type in .undefined
    if (ref.toInterned()) |interned_idx| {
        const ip_idx: u32 = @intFromEnum(interned_idx);
        const ty = ip.typeOf(interned_idx);
        const type_str = typeToString(null, null, arena, ip, ty);
        return clr_allocator.allocPrint(arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = .{{ .undefined = &{s} }} }} }}", .{ ip_idx, type_str }, null);
    }
    // Fallback - shouldn't happen for undefined stores
    return ".{ .interned = .{ .ip_idx = 0, .ty = .{ .undefined = &.{ .scalar = {} } } } }";
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

/// Check if an InternPool index is a well-known VALUE (not type) that shouldn't be looked up in IP.
/// These are values like .undef, .void_value, etc. that may not have valid IP entries in test contexts.
fn isWellKnownIndex(idx: InternPool.Index) bool {
    return switch (idx) {
        .none, .undef, .void_value, .unreachable_value, .null_value => true,
        .zero, .one, .negative_one, .zero_usize, .one_usize => true,
        .zero_u8, .bool_true, .bool_false => true,
        else => isWellKnownType(idx),
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

/// Public wrapper for typeToString for use by updateNav.
/// Doesn't register names since we don't have name maps in updateNav.
pub fn typeToStringForGlobal(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index) []const u8 {
    return typeToString(null, null, arena, ip, ty);
}

/// Inner function with cycle detection via visited set.
fn typeToStringInner(name_map: ?*std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
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
        // Many-pointers are pointer → region → element (same as slices)
        .manyptr_u8_type, .manyptr_const_u8_type, .manyptr_const_u8_sentinel_0_type => ".{ .pointer = &.{ .region = &.{ .scalar = {} } } }",
        // Slices are pointer → region → element
        .slice_const_u8_type, .slice_const_u8_sentinel_0_type => ".{ .pointer = &.{ .region = &.{ .scalar = {} } } }",
        // For other types, need to look up in InternPool
        else => if (name_map) |nm| typeToStringLookup(nm, field_map, arena, ip, ty, visited) else typeToStringLookupNoNames(arena, ip, ty, visited),
    };
}

/// Look up a non-well-known type without name registration (for callers without name_map).
fn typeToStringLookupNoNames(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const type_key = ip.indexToKey(ty);
    return switch (type_key) {
        .simple_type => |simple| switch (simple) {
            .void => ".{ .void = {} }",
            .noreturn => ".{ .void = {} }",
            else => ".{ .scalar = {} }",
        },
        .ptr_type => |ptr| blk: {
            // Function pointers get their own refinement type
            if (ip.indexToKey(ptr.child) == .func_type) {
                break :blk ".{ .fnptr = {} }";
            }
            const child_str = typeToStringInner(null, null, arena, ip, ptr.child, visited);
            // Slices and many-pointers are pointer → region → element
            if (ptr.flags.size == .slice or ptr.flags.size == .many) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .pointer = &.{{ .region = &{s} }} }}", .{child_str}, null);
            }
            break :blk clr_allocator.allocPrint(arena, ".{{ .pointer = &{s} }}", .{child_str}, null);
        },
        .opt_type => |child| blk: {
            const child_str = typeToStringInner(null, null, arena, ip, child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .optional = &{s} }}", .{child_str}, null);
        },
        .error_union_type => |eu| blk: {
            const payload_str = typeToStringInner(null, null, arena, ip, eu.payload_type, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .errorunion = &{s} }}", .{payload_str}, null);
        },
        .array_type => |arr| blk: {
            const child_str = typeToStringInner(null, null, arena, ip, arr.child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .region = &{s} }}", .{child_str}, null);
        },
        // Only struct/union types can cause cycles - check visited only for these
        .struct_type => blk: {
            if (visited.contains(ty)) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .recursive = {d} }}", .{@intFromEnum(ty)}, null);
            }
            visited.put(arena, ty, {}) catch @panic("out of memory");
            break :blk structTypeToStringSimple(arena, ip, ty, visited);
        },
        .union_type => blk: {
            if (visited.contains(ty)) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .recursive = {d} }}", .{@intFromEnum(ty)}, null);
            }
            visited.put(arena, ty, {}) catch @panic("out of memory");
            break :blk unionTypeToStringSimple(arena, ip, ty, visited);
        },
        // Enums and ints are scalars (we don't track them)
        // (floats come through .simple_type as f32_type, f64_type, etc.)
        .enum_type, .int_type => ".{ .scalar = {} }",
        // Bare function types (not behind a pointer) - function pointers are handled in .ptr_type
        .func_type => ".{ .unimplemented = {} }",
        else => ".{ .unimplemented = {} }",
    };
}

/// Simple union type string without name registration (for global variables)
/// Format: .{ .@"union" = &.{ .type_id = ID, .variants = &.{ TYPE_1, ... } } }
fn unionTypeToStringSimple(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadUnionType(type_index);
    const field_types = loaded.field_types.get(ip);
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"union\" = &.{{ .type_id = {d}, .variants = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - type too deeply nested
    if (visited.count() > 20) {
        return ".{ .unimplemented = {} }";
    }

    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"union\" = &.{{ .type_id = {d}, .variants = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }
        const field_type_str = typeToStringInner(null, null, arena, ip, field_type, visited);
        result.appendSlice(arena, field_type_str) catch @panic("out of memory");
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Simple struct type string without name registration (for global variables)
/// Format: .{ .@"struct" = &.{ .type_id = ID, .fields = &.{ TYPE_1, ... } } }
fn structTypeToStringSimple(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - type too deeply nested
    if (visited.count() > 20) {
        return ".{ .unimplemented = {} }";
    }

    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }
        const field_type_str = typeToStringInner(null, null, arena, ip, field_type, visited);
        result.appendSlice(arena, field_type_str) catch @panic("out of memory");
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Convert a type to string for global variables, with optional init value for per-field undefined tracking.
/// If init_val is an aggregate, checks each field element for .undef and wraps those field types.
pub fn typeToStringForGlobalWithInit(arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, init_val: InternPool.Index) []const u8 {
    // For struct types with aggregate init, check each field for undefined
    const type_key = ip.indexToKey(ty);
    if (type_key == .struct_type and init_val != .none) {
        const init_key = ip.indexToKey(init_val);
        if (init_key == .aggregate) {
            return structTypeToStringWithInit(arena, ip, ty, init_key.aggregate);
        }
    }
    // Fall back to standard conversion
    return typeToStringForGlobal(arena, ip, ty);
}

/// Struct type string with per-field undefined tracking based on init values.
/// Format: .{ .@"struct" = &.{ .type_id = ID, .fields = &.{ TYPE_1, ... } } }
fn structTypeToStringWithInit(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, agg: InternPool.Key.Aggregate) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{}} }} }}", .{type_id}, null);
    }

    // Get the init values for each field
    const field_values = switch (agg.storage) {
        .elems => |elems| elems,
        .repeated_elem, .bytes => return structTypeToStringSimpleNoVisited(arena, ip, type_index), // All same value or bytes, use simple path
    };

    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    var visited = VisitedTypes{};
    defer visited.deinit(arena);

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }
        // Check if this field's init value is undefined
        const field_is_undef = if (i < field_values.len) blk: {
            const field_val = field_values[i];
            break :blk ip.indexToKey(field_val) == .undef;
        } else false;

        const field_type_str = typeToStringInner(null, null, arena, ip, field_type, &visited);
        if (field_is_undef) {
            // Wrap in .undefined
            const wrapped = clr_allocator.allocPrint(arena, ".{{ .undefined = &{s} }}", .{field_type_str}, null);
            result.appendSlice(arena, wrapped) catch @panic("out of memory");
        } else {
            result.appendSlice(arena, field_type_str) catch @panic("out of memory");
        }
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Simple struct type string without visited tracking (for repeated_elem case)
fn structTypeToStringSimpleNoVisited(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index) []const u8 {
    var visited = VisitedTypes{};
    defer visited.deinit(arena);
    return structTypeToStringSimple(arena, ip, type_index, &visited);
}

/// Look up a non-well-known type in the InternPool.
fn typeToStringLookup(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, ty: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const type_key = ip.indexToKey(ty);
    return switch (type_key) {
        .simple_type => |simple| switch (simple) {
            .void => ".{ .void = {} }",
            .noreturn => ".{ .void = {} }",
            else => ".{ .scalar = {} }",
        },
        .ptr_type => |ptr| blk: {
            // Function pointers get their own refinement type
            if (ip.indexToKey(ptr.child) == .func_type) {
                break :blk ".{ .fnptr = {} }";
            }
            const child_str = typeToStringInner(name_map, field_map, arena, ip, ptr.child, visited);
            // Slices and many-pointers are pointer → region → element
            if (ptr.flags.size == .slice or ptr.flags.size == .many) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .pointer = &.{{ .region = &{s} }} }}", .{child_str}, null);
            }
            break :blk clr_allocator.allocPrint(arena, ".{{ .pointer = &{s} }}", .{child_str}, null);
        },
        .opt_type => |child| blk: {
            // opt_type payload is the child type directly (not a struct with .child)
            const child_str = typeToStringInner(name_map, field_map, arena, ip, child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .optional = &{s} }}", .{child_str}, null);
        },
        .error_union_type => |eu| blk: {
            // error_union_type has payload_type field
            const payload_str = typeToStringInner(name_map, field_map, arena, ip, eu.payload_type, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .errorunion = &{s} }}", .{payload_str}, null);
        },
        .array_type => |arr| blk: {
            const child_str = typeToStringInner(name_map, field_map, arena, ip, arr.child, visited);
            break :blk clr_allocator.allocPrint(arena, ".{{ .region = &{s} }}", .{child_str}, null);
        },
        // Only struct/union types can cause cycles - check visited only for these
        .struct_type => blk: {
            if (visited.contains(ty)) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .recursive = {d} }}", .{@intFromEnum(ty)}, null);
            }
            visited.put(arena, ty, {}) catch @panic("out of memory");
            break :blk structTypeToString(name_map, field_map, arena, ip, ty, visited);
        },
        .union_type => blk: {
            if (visited.contains(ty)) {
                break :blk clr_allocator.allocPrint(arena, ".{{ .recursive = {d} }}", .{@intFromEnum(ty)}, null);
            }
            visited.put(arena, ty, {}) catch @panic("out of memory");
            break :blk unionTypeToString(name_map, field_map, arena, ip, ty, visited);
        },
        // Enums and ints are scalars (we don't track them)
        // (floats come through .simple_type as f32_type, f64_type, etc.)
        .enum_type, .int_type => ".{ .scalar = {} }",
        // Bare function types (not behind a pointer) - function pointers are handled in .ptr_type
        .func_type => ".{ .unimplemented = {} }",
        else => ".{ .unimplemented = {} }",
    };
}

/// Convert a struct type to a Type string with type_id and field types.
/// Format: .{ .@"struct" = &.{ .type_id = ID, .fields = &.{ TYPE_1, TYPE_2, ... } } }
/// Field names are registered as a side effect into name_map and field_map.
fn structTypeToString(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);

    // Use InternPool index as unique type_id for this struct type
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - type too deeply nested
    if (visited.count() > 20) {
        return ".{ .unimplemented = {} }";
    }

    // Build field types string
    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Get field name and register it using InternPool index (side effect)
        const field_name_id: u32 = if (loaded.fieldName(ip, i).unwrap()) |name_str| blk: {
            const name_id = @intFromEnum(name_str);
            registerNameWithId(name_map, name_id, name_str.toSlice(ip));
            break :blk name_id;
        } else 0;

        // Register field mapping: {type_id, field_index} -> field_name_id
        if (field_map) |fm| {
            registerFieldName(fm, type_id, @intCast(i), field_name_id);
        }

        // Get the inner type string
        const field_type_str = typeToStringInner(name_map, field_map, arena, ip, field_type, visited);
        result.appendSlice(arena, field_type_str) catch @panic("out of memory");
    }

    result.appendSlice(arena, " } } }") catch @panic("out of memory");
    return result.items;
}

/// Convert a union type to a Type string with type_id and variant types.
/// Format: .{ .@"union" = &.{ .type_id = ID, .variants = &.{ TYPE_1, TYPE_2, ... } } }
/// Field names are registered as a side effect into name_map and field_map.
fn unionTypeToString(name_map: *std.AutoHashMapUnmanaged(u32, []const u8), field_map: ?*clr.FieldHashMap, arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, visited: *VisitedTypes) []const u8 {
    const loaded = ip.loadUnionType(type_index);
    const field_types = loaded.field_types.get(ip);

    // Use InternPool index as unique type_id for this union type
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"union\" = &.{{ .type_id = {d}, .variants = &.{{}} }} }}", .{type_id}, null);
    }

    // Limit recursion depth - type too deeply nested
    if (visited.count() > 20) {
        return ".{ .unimplemented = {} }";
    }

    // Build variant types string
    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"union\" = &.{{ .type_id = {d}, .variants = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Get field name from union's tag type and register it using InternPool index (side effect)
        const field_name_id: u32 = if (getUnionFieldNameInfo(ip, loaded.enum_tag_ty, i)) |info| blk: {
            registerNameWithId(name_map, info.id, info.name);
            break :blk info.id;
        } else 0;

        // Register field mapping: {type_id, field_index} -> field_name_id
        if (field_map) |fm| {
            registerFieldName(fm, type_id, @intCast(i), field_name_id);
        }

        // Get the inner type string
        const field_type_str = typeToStringInner(name_map, field_map, arena, ip, field_type, visited);
        result.appendSlice(arena, field_type_str) catch @panic("out of memory");
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
/// Format: .{ .@"struct" = &.{ .type_id = ID, .fields = &.{ TYPE_OR_UNDEFINED, ... } } }
fn aggregateValueToString(arena: std.mem.Allocator, ip: *const InternPool, type_index: InternPool.Index, agg: InternPool.Key.Aggregate) []const u8 {
    const loaded = ip.loadStructType(type_index);
    const field_types = loaded.field_types.get(ip);
    const field_values = agg.storage.values();
    const type_id: u32 = @intFromEnum(type_index);

    if (field_types.len == 0) {
        return clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{}} }} }}", .{type_id}, null);
    }

    var visited = VisitedTypes{};

    // Build field types string with undefined info from values
    var result = std.ArrayListUnmanaged(u8){};
    const header = clr_allocator.allocPrint(arena, ".{{ .@\"struct\" = &.{{ .type_id = {d}, .fields = &.{{ ", .{type_id}, null);
    result.appendSlice(arena, header) catch @panic("out of memory");

    for (field_types, 0..) |field_type, i| {
        if (i > 0) {
            result.appendSlice(arena, ", ") catch @panic("out of memory");
        }

        // Check if this field's value is undefined
        const is_field_undef = if (i < field_values.len) ip.isUndef(field_values[i]) else false;

        const inner_type_str = typeToStringInner(null, null, arena, ip, field_type, &visited);
        // If field is undefined, wrap it in .undefined
        const field_type_str = if (is_field_undef)
            clr_allocator.allocPrint(arena, ".{{ .undefined = &{s} }}", .{inner_type_str}, null)
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

    // Get ptr string - can be .inst or .interned
    const ptr_str: []const u8 = if (datum.bin_op.lhs.toIndex()) |idx|
        clr_allocator.allocPrint(info.arena, ".{{ .inst = {d} }}", .{@intFromEnum(idx)}, null)
    else if (datum.bin_op.lhs.toInterned()) |interned_idx| blk: {
        // Register globals (side effect)
        _ = tryFieldPtrRef(info, interned_idx);
        _ = tryGlobalRef(info, interned_idx);
        const ip_idx: u32 = @intFromEnum(interned_idx);
        const ty = info.ip.typeOf(interned_idx);
        const ty_str = typeToString(null, null, info.arena, info.ip, ty);
        break :blk clr_allocator.allocPrint(info.arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = {s} }} }}", .{ ip_idx, ty_str }, null);
    } else ".{ .interned = .{ .ip_idx = 0, .ty = .{ .unimplemented = {} } } }";

    // Check if storing an interned Allocator value - emit special allocator source
    if (datum.bin_op.rhs.toInterned()) |interned_idx| {
        const ty = info.ip.typeOf(interned_idx);
        if (isAllocatorType(info.ip, ty)) {
            // Extract the allocator type ID from the vtable
            const alloc_info = extractAllocatorTypeFromInterned(info.ip, interned_idx);
            const rhs_ip_idx: u32 = @intFromEnum(interned_idx);
            return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {s}, .src = .{{ .interned = .{{ .ip_idx = {d}, .ty = .{{ .allocator = {d} }} }} }} }}", .{ ptr_str, rhs_ip_idx, alloc_info.id }, null);
        }
    }

    const src_str = if (is_undef)
        // Wrap the type in .undefined when storing undefined
        srcStringUndef(info.arena, info.ip, datum.bin_op.rhs)
    else
        srcString(info, datum.bin_op.rhs);
    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {s}, .src = {s} }}", .{ ptr_str, src_str }, null);
}

fn payloadLoad(info: *const FnInfo, datum: Data) []const u8 {
    // ty_op.ty is a Ref to the result type (used for fallback type)
    const fallback_ty_str = if (datum.ty_op.ty.toInterned()) |ty_idx|
        typeToString(info.name_map, info.field_map, info.arena, info.ip, ty_idx)
    else
        ".{ .unimplemented = {} }";

    // Check for .none first (toIndex asserts on .none)
    if (datum.ty_op.operand == .none) {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = .{{ .interned = .{{ .ip_idx = 0, .ty = {s} }} }} }}", .{fallback_ty_str}, null);
    }
    if (datum.ty_op.operand.toIndex()) |idx| {
        // Runtime instruction index
        const ptr = @intFromEnum(idx);
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = .{{ .inst = {d} }} }}", .{ptr}, null);
    } else if (datum.ty_op.operand.toInterned()) |interned_idx| {
        // Register globals (side effect)
        _ = tryGlobalRef(info, interned_idx);
        const ip_idx: u32 = @intFromEnum(interned_idx);
        const ty = info.ip.typeOf(interned_idx);
        const ty_str = typeToString(null, null, info.arena, info.ip, ty);
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = .{{ .interned = .{{ .ip_idx = {d}, .ty = {s} }} }} }}", .{ ip_idx, ty_str }, null);
    } else {
        return clr_allocator.allocPrint(info.arena, ".{{ .ptr = .{{ .interned = .{{ .ip_idx = 0, .ty = {s} }} }} }}", .{fallback_ty_str}, null);
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
        return ".{ .ty = .{ .unimplemented = {} } }";
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
    const ptr_src = srcString(info, datum.bin_op.lhs);

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
        ".{ .unimplemented = {} }"; // Fallback for untagged unions or errors

    return clr_allocator.allocPrint(info.arena, ".{{ .ptr = {s}, .field_index = {?d}, .ty = {s} }}", .{ ptr_src, field_index, ty_str }, null);
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
    return_type: []const u8,
};

fn payloadCallParts(info: *const FnInfo, datum: Data) CallParts {
    // Call uses pl_op: operand is callee, payload indexes into extra
    // extra[payload] is args_len, followed by args_len Refs
    const callee_ref = datum.pl_op.operand;
    const payload_index = datum.pl_op.payload;
    const args_len = info.extra[payload_index];

    // Get called function and return type
    const called_str, const return_type_str = if (callee_ref.toIndex()) |idx| blk: {
        // Indirect call through function pointer (load, slice_elem_val, etc.)
        const callee_idx = @intFromEnum(idx);
        // Format as IndirectCall struct so Inst.call can identify indirect calls
        const called = clr_allocator.allocPrint(info.arena, "Inst.IndirectCall{{ .inst = {d} }}", .{callee_idx}, null);
        // Get the fnptr's type to extract return type
        const ret_type = getIndirectCallReturnType(info, callee_idx);
        break :blk .{ called, ret_type };
    } else if (callee_ref.toInterned()) |ip_idx| blk: {
        // Check if this is a function pointer constant (&function_name)
        const resolved_func_idx = extractFunctionFromPointer(info.ip, ip_idx) orelse ip_idx;
        // Add to call targets so the function is included in tree shaking
        info.call_targets.append(clr_allocator.allocator(), .{
            .index = @intFromEnum(resolved_func_idx),
            .arity = args_len,
        }) catch {};
        // Direct call to known function - use InternPool index as func_index
        const called = clr_allocator.allocPrint(info.arena, "fn_{d}", .{@intFromEnum(resolved_func_idx)}, null);
        const ret_type = extractFunctionReturnType(info.name_map, info.field_map, info.arena, info.ip, resolved_func_idx);
        break :blk .{ called, ret_type };
    } else .{ "null", ".{ .unimplemented = {} }" };

    // Build args slice string: &[_]Src{ Arg, Arg, ... }
    // Args are tagged unions: .{ .inst = N } or .{ .interned = ... }
    var args_str: []const u8 = "&[_]Src{";
    var first = true;

    var i: u32 = 0;
    while (i < args_len) : (i += 1) {
        const arg_ref: Ref = @enumFromInt(info.extra[payload_index + 1 + i]);
        if (arg_ref.toIndex()) |idx| {
            // Runtime value from local instruction - pass instruction index
            // srcSliceToGidSlice will look up results[idx].refinement at runtime
            const inst_idx = @intFromEnum(idx);
            if (first) {
                args_str = clr_allocator.allocPrint(info.arena, "{s} Src{{ .inst = {d} }}", .{ args_str, inst_idx }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(info.arena, "{s}, Src{{ .inst = {d} }}", .{ args_str, inst_idx }, null);
            }
        } else if (arg_ref.toInterned()) |interned_idx| {
            // Skip zero-sized types - they have no runtime representation
            const val_type = info.ip.typeOf(interned_idx);
            if (isZeroSizedType(info.ip, val_type)) continue;
            // Use srcString to properly handle globals, function pointers, etc.
            // srcString returns ".{ .xxx = ... }" - strip leading "." for struct init
            const src_str = srcString(info, arg_ref);
            const inner_str = if (src_str.len > 0 and src_str[0] == '.') src_str[1..] else src_str;
            if (first) {
                args_str = clr_allocator.allocPrint(info.arena, "{s} Src{s}", .{ args_str, inner_str }, null);
                first = false;
            } else {
                args_str = clr_allocator.allocPrint(info.arena, "{s}, Src{s}", .{ args_str, inner_str }, null);
            }
        }
    }
    args_str = clr_allocator.allocPrint(info.arena, "{s} }}", .{args_str}, null);

    return .{ .called = called_str, .args = args_str, .return_type = return_type_str };
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
        => datum.ty_op.ty.toInterned(),
        // Block and aggregate_init store result type in ty_pl.ty
        .block, .aggregate_init => datum.ty_pl.ty.toInterned(),
        else => null,
    };
}

/// Get the return type string for an indirect call through a function pointer.
/// The callee_idx is the instruction that produces the function pointer value.
fn getIndirectCallReturnType(info: *const FnInfo, callee_idx: usize) []const u8 {
    // Get the type of the instruction that produces the fnptr
    const fnptr_type = getInstResultType(info.ip, info.tags, info.data, callee_idx) orelse {
        return ".{ .unimplemented = {} }";
    };

    // The type should be a pointer to a function
    const type_key = info.ip.indexToKey(fnptr_type);
    const func_type_idx = switch (type_key) {
        .ptr_type => |pt| pt.child, // Dereference pointer to get function type
        .func_type => fnptr_type, // Already a function type (shouldn't happen but handle it)
        else => return ".{ .unimplemented = {} }",
    };

    // Get the function type to access return_type
    const func_type_key = info.ip.indexToKey(func_type_idx);
    const return_type_idx = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return ".{ .unimplemented = {} }",
    };

    // Convert return type to string
    return typeToString(info.name_map, info.field_map, info.arena, info.ip, return_type_idx);
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

/// Generate parameter list string - unified signature with args slice
fn buildParamList(arena: std.mem.Allocator, arg_count: u32) []const u8 {
    _ = arena;
    // Unified FnInterpreter signature: all functions take args: []const Gid
    // Inst.call converts Src slice to Gid slice before calling
    // Use named parameter if function has args, anonymous if not
    if (arg_count == 0) return ", _: []const Gid";
    return ", args: []const Gid";
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

/// Check if an interned function reference is an Allocator.alloc call (slice allocation)
fn isAllocatorAlloc(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorAllocFqn(fqn);
}

/// Check if an interned function reference is an Allocator.free call (slice deallocation)
fn isAllocatorFree(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorFreeFqn(fqn);
}

/// Check if an interned function reference is an Allocator.alignedAlloc call
fn isAllocatorAlignedAlloc(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorAlignedAllocFqn(fqn);
}

/// Check if an interned function reference is an Allocator.resize call
fn isAllocatorResize(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorResizeFqn(fqn);
}

/// Check if an interned function reference is an Allocator.remap call
fn isAllocatorRemap(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorRemapFqn(fqn);
}

/// Check if an interned function reference is an Allocator.realloc call
fn isAllocatorRealloc(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorReallocFqn(fqn);
}

/// Check if an interned function reference is an Allocator.dupe call
fn isAllocatorDupe(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorDupeFqn(fqn);
}

/// Check if an interned function reference is an Allocator.dupeZ call
fn isAllocatorDupeZ(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isAllocatorDupeZFqn(fqn);
}

/// Check if an interned function reference is an ArenaAllocator.deinit call
fn isArenaDeinit(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isArenaDeinitFqn(fqn);
}

/// Check if an interned function reference is an ArenaAllocator.allocator call
fn isArenaAllocator(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isArenaAllocatorFqn(fqn);
}

/// Check if an interned function reference is an ArenaAllocator.init call
fn isArenaInit(ip: *const InternPool, datum: Data) bool {
    const fqn = getCallFqn(ip, datum) orelse return false;
    return isArenaInitFqn(fqn);
}

/// Extract FQN from a call instruction's callee reference.
/// Strips __anon_XXXX suffix from generic instantiations so shims can match base names.
fn getCallFqn(ip: *const InternPool, datum: Data) ?[]const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return null;

    const key = ip.indexToKey(ip_idx);
    switch (key) {
        .func => |func_key| {
            const nav = ip.getNav(func_key.owner_nav);
            const fqn = nav.fqn.toSlice(ip);
            return stripAnonSuffix(fqn);
        },
        else => return null,
    }
}

/// Strip __anon_XXXX suffix from an FQN.
/// Generic instantiations get suffixes like "mem.eql__anon_4163".
/// We strip these so shims can match the base name "mem.eql".
fn stripAnonSuffix(fqn: []const u8) []const u8 {
    // Look for "__anon_" pattern
    const anon_marker = "__anon_";
    if (std.mem.indexOf(u8, fqn, anon_marker)) |idx| {
        return fqn[0..idx];
    }
    return fqn;
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
                                                const fqn = nav.fqn.toSlice(ip);
                                                // FQN is like "heap.PageAllocator.vtable"
                                                // Use FQN hash as type_id for shim compatibility
                                                const fqn_id = fqnTypeId(fqn);
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

/// Extract the allocator instruction index from the first argument to create/destroy.
/// Returns null if the allocator is interned (comptime) - in that case, type_id is reliable.
/// Returns the instruction index if the allocator comes from an instruction (runtime) -
/// in that case, the runtime handler should check the refinement for the actual type_id.
fn extractAllocatorInst(datum: Data, extra: []const u32) ?usize {
    const payload_index = datum.pl_op.payload;
    const args_len = extra[payload_index];
    if (args_len == 0) return null;

    // First argument is the allocator (self)
    const arg_ref: Ref = @enumFromInt(extra[payload_index + 1]);

    // If it's an interned value (comptime allocator), we don't need the inst
    if (arg_ref.toInterned() != null) return null;

    // It's an inst_ref - return the instruction index
    if (arg_ref.toIndex()) |inst_idx| {
        return @intFromEnum(inst_idx);
    }

    return null;
}

/// Extract the arena alloc instruction from arena.deinit() call.
/// The first argument is the loaded arena value - trace back through the load
/// to find the source pointer (the arena alloc instruction).
fn extractArenaInstFromDeinit(info: *const FnInfo, datum: Data) ?usize {
    const payload_index = datum.pl_op.payload;
    const args_len = info.extra[payload_index];
    if (args_len == 0) return null;

    // First argument is the loaded arena value
    const arg_ref: Ref = @enumFromInt(info.extra[payload_index + 1]);

    // Get the instruction index of the load
    const load_inst_idx = arg_ref.toIndex() orelse return null;
    const load_idx: usize = @intFromEnum(load_inst_idx);

    // Check if it's a load instruction
    if (info.tags[load_idx] != .load) return null;

    // Extract the source pointer from the load instruction
    // Load has ty_op data: .ty is the type, .operand is the source pointer
    const load_datum = info.data[load_idx];
    const src_ref = load_datum.ty_op.operand;

    // Get the instruction index of the source pointer (the arena alloc)
    if (src_ref.toIndex()) |src_idx| {
        return @intFromEnum(src_idx);
    }

    return null;
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

/// Check if FQN is an Allocator.alloc call (testable without InternPool)
/// Matches patterns like "mem.Allocator.alloc" for slice allocation
pub fn isAllocatorAllocFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.alloc") != null;
}

/// Check if FQN is an Allocator.free call (testable without InternPool)
/// Matches patterns like "mem.Allocator.free" for slice deallocation
pub fn isAllocatorFreeFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.free") != null;
}

/// Check if FQN is an Allocator.alignedAlloc call (testable without InternPool)
/// Note: alignedAlloc FQN doesn't match the generic "mem.Allocator.alloc" pattern
pub fn isAllocatorAlignedAllocFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.alignedAlloc") != null;
}

/// Check if FQN is an Allocator.resize call (testable without InternPool)
/// resize returns bool - in-place resize attempt
pub fn isAllocatorResizeFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.resize") != null;
}

/// Check if FQN is an Allocator.remap call (testable without InternPool)
/// remap returns ?[]T - resize with potential relocation
pub fn isAllocatorRemapFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.remap") != null;
}

/// Check if FQN is an Allocator.realloc call (testable without InternPool)
/// realloc returns Error![]T - full reallocation
pub fn isAllocatorReallocFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.realloc") != null;
}

/// Check if FQN is an Allocator.dupe call (testable without InternPool)
/// dupe returns Error![]T - allocates copy of slice
pub fn isAllocatorDupeFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.dupe") != null;
}

/// Check if FQN is an Allocator.dupeZ call (testable without InternPool)
/// dupeZ returns Error![:0]T - allocates copy with null terminator
pub fn isAllocatorDupeZFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.dupeZ") != null;
}

/// Compute a type_id from an FQN string.
/// Uses a hash to ensure consistent IDs between codegen and runtime shims.
/// The hash is truncated to u32 for compatibility with existing type_id fields.
pub fn fqnTypeId(fqn: []const u8) u32 {
    return @truncate(std.hash.Wyhash.hash(0, fqn));
}

/// Check if FQN is an ArenaAllocator.deinit call (testable without InternPool)
pub fn isArenaDeinitFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.deinit") != null;
}

/// Check if FQN is an ArenaAllocator.allocator call (testable without InternPool)
pub fn isArenaAllocatorFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.allocator") != null;
}

/// Check if FQN is an ArenaAllocator.init call (testable without InternPool)
pub fn isArenaInitFqn(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.init") != null;
}

/// Get the return type string for a call instruction's callee.
fn getCallReturnType(info: *const FnInfo, datum: Data) []const u8 {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return ".{ .unimplemented = {} }";
    const resolved_func_idx = extractFunctionFromPointer(info.ip, ip_idx) orelse ip_idx;
    return extractFunctionReturnType(info.name_map, info.field_map, info.arena, info.ip, resolved_func_idx);
}

/// Check if a type (by InternPool index) is std.mem.Allocator.
/// Returns true if the type is a struct named "mem.Allocator".
pub fn isAllocatorType(ip: *const InternPool, type_idx: InternPool.Index) bool {
    if (type_idx == .none) return false;
    // Skip built-in types/values (they can't be std.mem.Allocator)
    // Built-in indices go from 0 to empty_tuple (last predefined value)
    if (@intFromEnum(type_idx) <= @intFromEnum(InternPool.Index.empty_tuple)) return false;
    const type_key = ip.indexToKey(type_idx);
    switch (type_key) {
        .struct_type => {
            const loaded = ip.loadStructType(type_idx);
            const name = loaded.name.toSlice(ip);
            return std.mem.eql(u8, name, "mem.Allocator");
        },
        else => return false,
    }
}

/// Check if a call returns std.mem.Allocator and return the allocator type info.
/// Returns null if the call doesn't return Allocator.
fn getCallAllocatorReturnInfo(ip: *const InternPool, datum: Data) ?AllocatorTypeInfo {
    const callee_ref = datum.pl_op.operand;
    const ip_idx = callee_ref.toInterned() orelse return null;

    // Get function return type
    const func_key = ip.indexToKey(ip_idx);
    const func_ty = switch (func_key) {
        .func => |f| f.ty,
        else => return null,
    };

    const func_type_key = ip.indexToKey(func_ty);
    const return_type_idx = switch (func_type_key) {
        .func_type => |ft| ft.return_type,
        else => return null,
    };

    // Check if return type is std.mem.Allocator
    if (!isAllocatorType(ip, return_type_idx)) return null;

    // Get the callee's FQN to extract the allocator type
    const nav = switch (func_key) {
        .func => |f| ip.getNav(f.owner_nav),
        else => return null,
    };
    const fqn = nav.fqn.toSlice(ip);

    // FQN is like "heap.GeneralPurposeAllocator(...).allocator" or similar
    // Use FQN hash as type_id for shim compatibility
    const fqn_id = fqnTypeId(fqn);
    const type_name = extractTypeFromAllocatorMethod(fqn);

    return .{ .id = fqn_id, .name = type_name };
}

/// Extract allocator type info from an interned Allocator aggregate value.
/// Returns the type ID (from vtable nav FQN) and display name.
fn extractAllocatorTypeFromInterned(ip: *const InternPool, interned_idx: InternPool.Index) AllocatorTypeInfo {
    const val_key = ip.indexToKey(interned_idx);
    switch (val_key) {
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
                                            // Use FQN hash as type_id for shim compatibility
                                            const fqn_id = fqnTypeId(fqn);
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

/// Check if a Ref points to a value of type std.mem.Allocator.
/// Works for both interned values and instruction references.
pub fn isAllocatorRef(ip: *const InternPool, ref: Ref, tags: []const Tag, data: []const Data) bool {
    if (ref.toInterned()) |interned_idx| {
        const ty = ip.typeOf(interned_idx);
        return isAllocatorType(ip, ty);
    }
    if (ref.toIndex()) |inst_idx| {
        // Get the result type of the instruction
        const ty = getInstResultType(ip, tags, data, @intFromEnum(inst_idx)) orelse return false;
        return isAllocatorType(ip, ty);
    }
    return false;
}

/// Extract the pointer source being destroyed from a destroy call.
/// destroy(self, ptr) - the second argument (index 1) is the pointer.
/// Returns a Src string like ".{ .inst = N }" or ".{ .interned = ... }"
fn extractDestroyPtrSrc(info: *const FnInfo, datum: Data) ?[]const u8 {
    const payload_index = datum.pl_op.payload;
    const args_len = info.extra[payload_index];

    // destroy has 2 args: (self, ptr) - we want arg index 1
    if (args_len < 2) return null;

    const ptr_arg_ref: Ref = @enumFromInt(info.extra[payload_index + 1 + 1]); // +1 for args_len, +1 for second arg
    if (ptr_arg_ref.toIndex()) |idx| {
        return clr_allocator.allocPrint(info.arena, ".{{ .inst = {d} }}", .{@intFromEnum(idx)}, null);
    } else if (ptr_arg_ref.toInterned()) |interned_idx| {
        // Register globals (side effect)
        _ = tryGlobalRef(info, interned_idx);
        const ip_idx: u32 = @intFromEnum(interned_idx);
        const ty = info.ip.typeOf(interned_idx);
        const ty_str = typeToString(null, null, info.arena, info.ip, ty);
        return clr_allocator.allocPrint(info.arena, ".{{ .interned = .{{ .ip_idx = {d}, .ty = {s} }} }}", .{ ip_idx, ty_str }, null);
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
    loop_switch_case: struct {
        case_index: u32,
        num_cases: u32,
        loop_switch_inst: u32,
    },
    loop_body,
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
                .loop_switch_case => |lsc| clr_allocator.allocPrint(arena, "fn_{d}_loop_switch_case_{d}_{d}", .{
                    s.func_index,
                    lsc.case_index,
                    s.instr_index,
                }, null),
                .loop_body => clr_allocator.allocPrint(arena, "fn_{d}_loop_body_{d}", .{
                    s.func_index,
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
            // FnInterpreter signature: *Context, *Refinements, Gid, []const Gid
            .full => |f| clr_allocator.allocPrint(arena,
                \\fn fn_{d}(ctx: *Context, refinements: *Refinements, return_gid: Gid{s}) anyerror!Gid {{
                \\    ctx.meta.file = "{s}";
                \\    ctx.base_line = {d};
                \\    try ctx.push_fn("{s}");
                \\    defer ctx.pop_fn();
                \\    refinements.testValid();
                \\
                \\    const results = try Inst.make_results_list(ctx.allocator, {d});
                \\    defer Inst.clear_results_list(results, ctx.allocator);
                \\
                \\    var early_returns = @import("std").ArrayListUnmanaged(State){{}};
                \\    defer Inst.freeEarlyReturns(&early_returns, ctx.allocator);
                \\
                \\    const base_gid: Gid = @intCast(refinements.list.items.len);
                \\    const state = State{{ .ctx = ctx, .results = results, .refinements = refinements, .return_gid = return_gid, .base_gid = base_gid, .early_returns = &early_returns }};
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
                .loop_switch_case => |lsc| clr_allocator.allocPrint(arena,
                    \\fn fn_{d}_loop_switch_case_{d}_{d}(state: State) anyerror!void {{
                    \\
                , .{ s.func_index, lsc.case_index, s.instr_index }, null),
                .loop_body => clr_allocator.allocPrint(arena,
                    \\fn fn_{d}_loop_body_{d}(state: State) anyerror!void {{
                    \\
                , .{ s.func_index, s.instr_index }, null),
            },
        };
    }

    /// Generate function footer (closing)
    fn footer(self: FunctionGen, arena: std.mem.Allocator) []const u8 {
        return switch (self) {
            .full => clr_allocator.allocPrint(arena,
                \\    try Inst.mergeEarlyReturns(state);
                \\    try Inst.onFinish(state);
                \\    return return_gid;
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
    const payload_index = block_data.ty_pl.payload;
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
    /// Field index if this case matches an enum tag (for union or enum switch)
    field_index: ?u32 = null,
    /// First item Ref for this case (used to match switch_dispatch targets)
    first_item: ?Ref = null,
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

        // Extract field_index and first_item from first item (for any switch with items)
        var field_index: ?u32 = null;
        var first_item: ?Ref = null;
        if (items_len > 0) {
            // Items are Ref values stored as u32s
            const item_ref: Ref = @enumFromInt(extra[extra_index]);
            first_item = item_ref;
            if (item_ref.toInterned()) |interned_idx| {
                field_index = extractEnumTagFieldIndex(ip, interned_idx);
            }
        }

        // Skip items and ranges
        extra_index += items_len;
        extra_index += ranges_len * 2; // ranges are pairs

        // Extract body
        if (extra_index + body_len > extra.len) return null;
        cases[i] = .{ .body = extra[extra_index..][0..body_len], .field_index = field_index, .first_item = first_item };
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
    // buildParamList returns ", _: []const Src" or ", args: []const Src"
    const arg_count = countArgs(info.tags);
    const params = buildParamList(info.arena, arg_count);

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

                // Check for loop_switch_br - add all cases to worklist (similar to switch_br)
                if (tag == .loop_switch_br) {
                    if (extractSwitchBrBodies(info.arena, info.ip, idx, info.tags, info.data, info.extra)) |bodies| {
                        const has_else = bodies.else_body.len > 0;
                        const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                        // Add each case
                        for (bodies.cases, 0..) |case, case_idx| {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .loop_switch_case = .{
                                    .case_index = @intCast(case_idx),
                                    .num_cases = num_cases,
                                    .loop_switch_inst = idx,
                                } },
                                .body_indices = case.body,
                            } }) catch @panic("out of memory");
                        }
                        // Add else case
                        if (has_else) {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .loop_switch_case = .{
                                    .case_index = @intCast(bodies.cases.len),
                                    .num_cases = num_cases,
                                    .loop_switch_inst = idx,
                                } },
                                .body_indices = bodies.else_body,
                            } }) catch @panic("out of memory");
                        }
                    }
                }

                // Check for loop - add loop body to worklist
                // Loop uses the same Block payload structure as block
                if (tag == .loop) {
                    if (extractBlockBody(idx, info.data, info.extra)) |body| {
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .loop_body,
                            .body_indices = body,
                        } }) catch @panic("out of memory");
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

                // Check for nested loop_switch_br
                if (tag == .loop_switch_br) {
                    if (extractSwitchBrBodies(info.arena, info.ip, idx, info.tags, info.data, info.extra)) |bodies| {
                        const has_else = bodies.else_body.len > 0;
                        const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                        for (bodies.cases, 0..) |case, case_idx| {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .loop_switch_case = .{
                                    .case_index = @intCast(case_idx),
                                    .num_cases = num_cases,
                                    .loop_switch_inst = idx,
                                } },
                                .body_indices = case.body,
                            } }) catch @panic("out of memory");
                        }
                        if (has_else) {
                            worklist.append(info.arena, .{ .sub = .{
                                .func_index = func_index,
                                .instr_index = idx,
                                .tag = .{ .loop_switch_case = .{
                                    .case_index = @intCast(bodies.cases.len),
                                    .num_cases = num_cases,
                                    .loop_switch_inst = idx,
                                } },
                                .body_indices = bodies.else_body,
                            } }) catch @panic("out of memory");
                        }
                    }
                }

                // Check for nested loop
                if (tag == .loop) {
                    if (extractBlockBody(idx, info.data, info.extra)) |body| {
                        worklist.append(info.arena, .{ .sub = .{
                            .func_index = func_index,
                            .instr_index = idx,
                            .tag = .loop_body,
                            .body_indices = body,
                        } }) catch @panic("out of memory");
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
            .loop_switch_case => |lsc| {
                // Inject LoopSwitchBr tag at the start of loop switch case
                const loop_switch_br_line = generateLoopSwitchBrTagLine(info.arena, lsc.case_index, lsc.num_cases, lsc.loop_switch_inst);
                lines.append(info.arena, loop_switch_br_line) catch @panic("out of memory");
                total_len += loop_switch_br_line.len;
            },
            .loop_body => {
                // No special tag line needed for loop body - loop handling is done in Inst.loop()
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
        } else if (instr.tag == .loop_switch_br) {
            // Generate Inst.loop_switch_br call with all case function references
            if (extractSwitchBrBodies(info.arena, info.ip, instr.idx, info.tags, info.data, info.extra)) |bodies| {
                const has_else = bodies.else_body.len > 0;
                const num_cases: u32 = @intCast(bodies.cases.len + @as(usize, if (has_else) 1 else 0));
                line = generateLoopSwitchBrLine(info.arena, instr.idx, func_index, num_cases);
            } else {
                // Fallback to noop if extraction fails
                line = generateNoopLine(info.arena, instr.idx);
            }
        } else if (instr.tag == .loop) {
            // Generate Inst.loop call with body function reference
            line = generateLoopLine(info.arena, instr.idx, func_index);
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

/// Generate the Inst.loop call line with body function reference
fn generateLoopLine(arena: std.mem.Allocator, loop_idx: u32, func_index: u32) []const u8 {
    return clr_allocator.allocPrint(arena,
        \\    try Inst.loop(state, {d}, fn_{d}_loop_body_{d});
        \\
    , .{ loop_idx, func_index, loop_idx }, null);
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

/// Generate Inst.loop_switch_br call with all case function references.
/// Similar to generateSwitchBrLine but for loop_switch_br (labeled switch with continue).
fn generateLoopSwitchBrLine(arena: std.mem.Allocator, switch_idx: u32, func_index: u32, num_cases: u32) []const u8 {
    // Build the tuple of function pointers: .{ fn_X_loop_switch_case_0_Y, fn_X_loop_switch_case_1_Y, ... }
    var case_fns: []const u8 = ".{";
    for (0..num_cases) |i| {
        if (i > 0) {
            case_fns = clr_allocator.allocPrint(arena, "{s}, fn_{d}_loop_switch_case_{d}_{d}", .{
                case_fns,
                func_index,
                i,
                switch_idx,
            }, null);
        } else {
            case_fns = clr_allocator.allocPrint(arena, "{s} fn_{d}_loop_switch_case_{d}_{d}", .{
                case_fns,
                func_index,
                i,
                switch_idx,
            }, null);
        }
    }
    case_fns = clr_allocator.allocPrint(arena, "{s} }}", .{case_fns}, null);

    return clr_allocator.allocPrint(arena,
        \\    try Inst.loop_switch_br(state, {d}, {s});
        \\
    , .{ switch_idx, case_fns }, null);
}

/// Generate LoopSwitchBr tag line at the start of loop switch case functions
fn generateLoopSwitchBrTagLine(arena: std.mem.Allocator, case_index: u32, num_cases: u32, loop_switch_inst: u32) []const u8 {
    return clr_allocator.allocPrint(arena,
        \\    try Inst.apply(state, 0, .{{ .loop_switch_br = .{{ .case_index = {d}, .num_cases = {d}, .loop_switch_inst = {d} }} }});
        \\
    , .{ case_index, num_cases, loop_switch_inst }, null);
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

/// Format ChildInfo union for code generation
fn formatChildInfo(children: clr.ChildInfo) []const u8 {
    const allocator = clr_allocator.allocator();
    return switch (children) {
        .scalar => ".{ .scalar = {} }",
        .null => ".{ .@\"null\" = {} }",
        .indirect => |target| if (target) |t|
            clr_allocator.allocPrint(allocator, ".{{ .indirect = {d} }}", .{t}, null)
        else
            ".{ .indirect = null }",
        .struct_fields => |fields| blk: {
            var buf = std.ArrayListUnmanaged(u8){};
            buf.appendSlice(allocator, ".{ .struct_fields = &.{ ") catch @panic("OOM");
            for (fields, 0..) |field, i| {
                if (i > 0) buf.appendSlice(allocator, ", ") catch @panic("OOM");
                const field_str = if (field) |f|
                    clr_allocator.allocPrint(allocator, "{d}", .{f}, null)
                else
                    "null";
                buf.appendSlice(allocator, field_str) catch @panic("OOM");
            }
            buf.appendSlice(allocator, " } }") catch @panic("OOM");
            break :blk buf.items;
        },
        .union_field => |uf| blk: {
            const active_str = if (uf.active_field_index) |idx|
                clr_allocator.allocPrint(allocator, "@as(?usize, {d})", .{idx}, null)
            else
                "null";
            const field_value_str = if (uf.field_value_ip_idx) |idx|
                clr_allocator.allocPrint(allocator, "@as(?u32, {d})", .{idx}, null)
            else
                "null";
            break :blk clr_allocator.allocPrint(allocator, ".{{ .union_field = .{{ .active_field_index = {s}, .num_fields = {d}, .field_value_ip_idx = {s} }} }}", .{ active_str, uf.num_fields, field_value_str }, null);
        },
    };
}

/// Generate epilogue with imports and main function
pub fn epilogue(entrypoint_index: u32, return_type: ?[]const u8) []u8 {
    // Generate the global_defs array from registered globals
    const globals_count = clr.getGlobalsCount();
    var global_defs_str = std.ArrayListUnmanaged(u8){};
    global_defs_str.appendSlice(clr_allocator.allocator(), "const global_defs = [_]clr.GlobalDef{") catch @panic("out of memory");
    var it = clr.getGlobalsIterator();
    while (it.next()) |g| {
        // Format location info
        const loc_str = if (g.loc) |loc|
            clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .file = \"{s}\", .line = {d}, .column = {d} }}", .{ loc.file, loc.line, loc.column }, null)
        else
            ".{}";

        // Format children info
        const children_str = formatChildInfo(g.children);

        // Format type_id (optional - only for struct/union globals)
        const type_id_str = if (g.type_id) |tid|
            clr_allocator.allocPrint(clr_allocator.allocator(), ", .type_id = {d}", .{tid}, null)
        else
            "";

        const entry = clr_allocator.allocPrint(clr_allocator.allocator(), "\n    .{{ .ip_idx = {d}, .ty = {s}{s}, .loc = {s}, .children = {s} }},", .{ g.ip_idx, g.type_str, type_id_str, loc_str, children_str }, null);
        global_defs_str.appendSlice(clr_allocator.allocator(), entry) catch @panic("out of memory");
    }
    global_defs_str.appendSlice(clr_allocator.allocator(), "\n};\n") catch @panic("out of memory");

    // Generate the return slot initialization
    const return_slot_init = if (return_type) |rt|
        clr_allocator.allocPrint(clr_allocator.allocator(),
            \\    const return_type: clr.Type = {s};
            \\    const return_ref = clr.typeToRefinement(return_type, &refinements) catch Refinements.Refinement{{ .scalar = .{{}} }};
            \\    const return_gid = refinements.appendEntity(return_ref) catch 0;
            \\    clr.splatInit(&refinements, return_gid, &ctx);
        , .{rt}, null)
    else
        @panic("entrypoint must have return type");

    // Choose init function based on whether we have globals
    const init_call = if (globals_count > 0)
        "    var refinements = Refinements.initWithGlobals(allocator, &ctx, &global_defs);"
    else
        "    var refinements = Refinements.init(allocator);";

    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\const std = @import("std");
        \\const clr = @import("clr");
        \\const Context = clr.Context;
        \\const Inst = clr.Inst;
        \\const Refinements = clr.Refinements;
        \\const Gid = clr.Gid;
        \\const Src = clr.Src;
        \\const State = clr.State;
        \\
        \\{s}
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
        \\
        \\{s}
        \\    defer refinements.deinit();
        \\{s}
        \\
        \\    _ = fn_{d}(&ctx, &refinements, return_gid, &.{{}}) catch {{
        \\        file_writer.interface.flush() catch {{}};
        \\        std.process.exit(1);
        \\    }};
        \\    refinements.testValid();
        \\}}
        \\
    , .{ global_defs_str.items, init_call, return_slot_init, entrypoint_index }, null);
}

/// Generate a stub function for a missing call target
pub fn generateStub(func_index: u32, arity: u32) []u8 {
    _ = arity; // Unified signature always uses args slice

    // FnInterpreter signature: *Context, *Refinements, Gid, []const Gid
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\fn fn_{d}(ctx: *Context, _: *Refinements, return_gid: Gid, _: []const Gid) anyerror!Gid {{
        \\    std.debug.print("WARNING: call to unresolved function fn_{d}\\n", .{{}});
        \\    ctx.dumpStackTrace();
        \\    return return_gid;
        \\}}
        \\
    , .{ func_index, func_index }, null);
}

// =============================================================================
// Compile-Time Function Interception
// =============================================================================
//
// Functions that will be intercepted at runtime should be stubbed at compile
// time to avoid generating full AIR for stdlib internals (syscalls, etc.).
// =============================================================================

/// Check if FQN contains pattern (simple substring search, no stdlib calls)
fn containsPattern(haystack: []const u8, needle: []const u8) bool {
    if (needle.len > haystack.len) return false;
    if (needle.len == 0) return true;

    var i: usize = 0;
    while (i + needle.len <= haystack.len) : (i += 1) {
        if (std.mem.eql(u8, haystack[i..][0..needle.len], needle)) {
            return true;
        }
    }
    return false;
}

/// Check if a function FQN matches patterns that will be intercepted at runtime.
pub fn shouldIntercept(fqn: []const u8) bool {
    // Allocator operations
    if (containsPattern(fqn, "mem.Allocator.create")) return true;
    if (containsPattern(fqn, "mem.Allocator.destroy")) return true;
    if (containsPattern(fqn, "mem.Allocator.alloc")) return true;
    if (containsPattern(fqn, "mem.Allocator.alignedAlloc")) return true;
    if (containsPattern(fqn, "mem.Allocator.free")) return true;
    if (containsPattern(fqn, "mem.Allocator.realloc")) return true;
    if (containsPattern(fqn, "mem.Allocator.resize")) return true;
    if (containsPattern(fqn, "mem.Allocator.remap")) return true;
    if (containsPattern(fqn, "mem.Allocator.dupe")) return true;
    // Arena operations
    if (containsPattern(fqn, "ArenaAllocator.init")) return true;
    if (containsPattern(fqn, "ArenaAllocator.deinit")) return true;
    if (containsPattern(fqn, "ArenaAllocator.allocator")) return true;
    // Posix operations (dive into syscalls)
    if (containsPattern(fqn, "posix.open")) return true;
    if (containsPattern(fqn, "posix.close")) return true;
    if (containsPattern(fqn, "posix.read")) return true;
    if (containsPattern(fqn, "posix.write")) return true;
    if (containsPattern(fqn, "posix.socket")) return true;
    if (containsPattern(fqn, "posix.accept")) return true;
    if (containsPattern(fqn, "posix.pipe")) return true;
    if (containsPattern(fqn, "posix.dup")) return true;
    if (containsPattern(fqn, "posix.epoll_create")) return true;
    if (containsPattern(fqn, "posix.pread")) return true;
    if (containsPattern(fqn, "posix.pwrite")) return true;
    // Formatter functions (comptime format string pattern)
    if (containsPattern(fqn, "fmt.format")) return true;
    if (containsPattern(fqn, "fmt.bufPrint")) return true;
    if (containsPattern(fqn, "fmt.count")) return true;
    if (containsPattern(fqn, "log.scoped")) return true;
    if (containsPattern(fqn, "log.default")) return true;
    return false;
}

/// Generate a stub for a function that will be intercepted at runtime.
/// Body panics - if executed, runtime interception failed (pattern drift).
pub fn generateInterceptedStub(func_index: u32, fqn: []const u8) []u8 {
    return clr_allocator.allocPrint(clr_allocator.allocator(),
        \\// Intercepted: {s}
        \\fn fn_{d}(_: *Context, _: *Refinements, _: Gid, _: []const Gid) anyerror!Gid {{
        \\    @panic("intercepted stub executed: {s} - runtime interception failed, check for pattern drift");
        \\}}
        \\
    , .{ fqn, func_index, fqn }, null);
}

const CallTarget = @import("clr.zig").CallTarget;

test {
    @import("std").testing.refAllDecls(@import("codegen_test.zig"));
}
