const std = @import("std");
const compiler = @import("compiler");
const Zcu = compiler.Zcu;
const Air = compiler.Air;
const InternPool = compiler.InternPool;
const Tag = Air.Inst.Tag;
const Data = Air.Inst.Data;
const codegen = compiler.codegen;
const link = compiler.link;
const air = codegen.importBackend(.stage2_air);
const VTable = air.VTable;
const Mir = air.Mir;
const debug = @import("debug.zig");

const c_anyopaque_t = ?[*]u8;
const c_anyopaque_const_t = ?[*]const u8;

// VTable must be `undefined` and initialized at runtime in init().
// Compile-time initialization would create function pointers that don't
// survive DLL relocation. See allocator.zig for full explanation.
var vtable: VTable = undefined;

const clr_allocator = @import("allocator.zig");
const clr_codegen = @import("codegen.zig");
const tree_shaker = @import("tree_shaker.zig");

pub const CallTarget = struct {
    index: u32,
    arity: u32,
};

/// Key for field name lookup: {type_id, field_index}
pub const FieldKey = struct {
    type_id: u32,
    field_index: u32,

    /// Hash context for AutoHashMap - XOR the two field hashes
    pub const HashContext = struct {
        pub fn hash(_: HashContext, key: FieldKey) u64 {
            return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.type_id)) ^
                std.hash.Wyhash.hash(0, std.mem.asBytes(&key.field_index));
        }

        pub fn eql(_: HashContext, a: FieldKey, b: FieldKey) bool {
            return a.type_id == b.type_id and a.field_index == b.field_index;
        }
    };
};

/// Mapping from {type_id, field_index} to name_id
pub const FieldMap = struct { FieldKey, u32 };

/// Hash map type for field mappings
pub const FieldHashMap = std.HashMapUnmanaged(FieldKey, u32, FieldKey.HashContext, std.hash_map.default_max_load_percentage);

/// Tuple that represents a InternPool index / name pairing.
pub const NameMap = struct { u32, []const u8 };

/// Per-function context for code generation.
/// Aggregates commonly-passed parameters to reduce function signatures.
pub const FnInfo = struct {
    arena: std.mem.Allocator,
    name_map: *std.AutoHashMapUnmanaged(u32, []const u8),
    field_map: *FieldHashMap,
    ip: *const InternPool,
    tags: []const Tag,
    data: []const Data,
    extra: []const u32,
    param_names: []const []const u8,
};

pub const FuncMir = struct {
    func_index: u32,
    owner_nav: u32,
    text: []const u8,
    call_targets: []const CallTarget,
    entrypoint: bool = false,
    name_mappings: []const NameMap,
    field_mappings: []const FieldMap,
};

// Collection of MIR structs
var mir_list: std.ArrayListUnmanaged(*const FuncMir) = .empty;

export fn init(avt: *const clr_allocator.AllocatorVTable) ?*const u8 {
    clr_allocator.init(avt);
    mir_list = .empty;
    vtable = .{
        .deinit = null,
        .mir_deinit = mirDeinit,
        .generate = generate,
        .link = .{
            .deinit = linkDeinit,
            .updateFunc = updateFunc,
            .updateNav = updateNav,
            .updateLineNumber = updateLineNumber,
            .flush = flush,
            .updateExports = updateExports,
            .deleteExport = deleteExport,
        },
    };
    return @ptrCast(&vtable);
}

fn mirDeinit(_: c_anyopaque_t) callconv(.c) void {}

/// Extract parameter names from ZIR for a function
fn extractParamNames(_: *const Zcu, ip: *const InternPool, func: InternPool.Key.Func, file_scope: *Zcu.File) []const []const u8 {
    const zir = file_scope.zir orelse return &.{};
    const zir_inst = func.zir_body_inst.resolve(ip) orelse return &.{};

    // Get the param body (list of param ZIR instructions)
    const param_body = zir.getParamBody(zir_inst);
    if (param_body.len == 0) return &.{};

    // Allocate array for names
    const names = clr_allocator.allocator().alloc([]const u8, param_body.len) catch return &.{};

    for (param_body, 0..) |param_inst, i| {
        if (zir.getParamName(param_inst)) |name_str| {
            names[i] = zir.nullTerminatedString(name_str);
        } else {
            names[i] = "";
        }
    }

    return names;
}

fn generate(_: c_anyopaque_t, pt_ptr: c_anyopaque_const_t, _: c_anyopaque_const_t, func_index: u32, air_ptr: c_anyopaque_const_t, _: c_anyopaque_const_t) callconv(.c) c_anyopaque_const_t {
    const pt: *const Zcu.PerThread = @ptrCast(@alignCast(pt_ptr));
    const func_air: *const Air = @ptrCast(@alignCast(air_ptr));

    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const func = zcu.funcInfo(@enumFromInt(func_index));
    const nav = ip.getNav(func.owner_nav);
    const fqn = nav.fqn.toSlice(ip);

    // Get root module name to filter user code from stdlib.
    const root_name = zcu.root_mod.fully_qualified_name;

    // Skip non-user functions (stdlib) - these crash navSrcLine/toAbsolute in Release
    if (!std.mem.startsWith(u8, fqn, root_name)) {
        return null;
    }

    // Check if this is the main function (entrypoint)
    var expected_main: [256]u8 = undefined;
    const expected_main_slice = std.fmt.bufPrint(&expected_main, "{s}.main", .{root_name}) catch return null;
    const is_entrypoint = std.mem.eql(u8, fqn, expected_main_slice);

    // Get instruction tags, data, and extra from AIR
    const tags = func_air.instructions.items(.tag);
    const data = func_air.instructions.items(.data);
    const extra = func_air.extra.items;

    // Get function's source location
    const base_line = zcu.navSrcLine(func.owner_nav);
    const file_scope = zcu.navFileScope(func.owner_nav);
    // Use sub_path directly to avoid toAbsolute crash in Release mode
    const file_path = file_scope.path.sub_path;

    // Extract parameter names from ZIR
    const param_names = extractParamNames(zcu, ip, func, file_scope);

    // Per-function arena for temporary allocations during code generation
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Create per-function name map (no global state - avoids race conditions)
    var name_map = std.AutoHashMapUnmanaged(u32, []const u8){};

    // Create per-function field map for {type_id, field_index} -> name_id
    var field_map = FieldHashMap{};

    // Create FnInfo struct to pass to generateFunction
    const info = FnInfo{
        .arena = arena.allocator(),
        .name_map = &name_map,
        .field_map = &field_map,
        .ip = ip,
        .tags = tags,
        .data = data,
        .extra = extra,
        .param_names = param_names,
    };

    // Generate Zig source for this function
    const text = clr_codegen.generateFunction(func_index, fqn, &info, base_line, file_path);

    // Convert hash maps to slices for storage on FuncMir
    const name_mappings = convertNameMapToSlice(&name_map);
    const field_mappings = convertFieldMapToSlice(&field_map);

    // Extract call targets from AIR (skips debug.* calls)
    const call_targets = clr_codegen.extractCallTargets(clr_allocator.allocator(), ip, tags, data, extra);

    const mir = clr_allocator.allocator().create(FuncMir) catch return null;
    mir.* = .{
        .func_index = func_index,
        .owner_nav = @intFromEnum(func.owner_nav),
        .text = text,
        .call_targets = call_targets,
        .entrypoint = is_entrypoint,
        .name_mappings = name_mappings,
        .field_mappings = field_mappings,
    };
    return @ptrCast(mir);
}

fn convertNameMapToSlice(map: *std.AutoHashMapUnmanaged(u32, []const u8)) []const NameMap {
    const allocator = clr_allocator.allocator();
    const result = allocator.alloc(NameMap, map.count()) catch return &.{};
    var i: usize = 0;
    var it = map.iterator();
    while (it.next()) |entry| {
        result[i] = .{ entry.key_ptr.*, entry.value_ptr.* };
        i += 1;
    }
    return result;
}

fn convertFieldMapToSlice(map: *FieldHashMap) []const FieldMap {
    const allocator = clr_allocator.allocator();
    const result = allocator.alloc(FieldMap, map.count()) catch return &.{};
    var i: usize = 0;
    var it = map.iterator();
    while (it.next()) |entry| {
        result[i] = .{ entry.key_ptr.*, entry.value_ptr.* };
        i += 1;
    }
    return result;
}

fn linkDeinit(_: c_anyopaque_t) callconv(.c) void {}

fn updateFunc(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32, mir_ptr: c_anyopaque_t) callconv(.c) void {
    const ptr = mir_ptr orelse return;
    // mir_ptr points to a Mir struct, extract .result to get our FuncMir
    const mir_wrapper: *const Mir = @ptrCast(@alignCast(ptr));
    const func_mir_ptr = mir_wrapper.result orelse return;
    const mir: *const FuncMir = @ptrCast(@alignCast(func_mir_ptr));
    mir_list.append(clr_allocator.allocator(), mir) catch return;
}

fn updateNav(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}

fn updateLineNumber(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}

fn flush(lf_ptr: c_anyopaque_t, _: c_anyopaque_const_t, _: u32, _: c_anyopaque_const_t) callconv(.c) void {
    const lf: *link.File = @ptrCast(@alignCast(lf_ptr));
    const file = lf.file orelse return;

    file.seekTo(0) catch return;

    // Find entrypoint first
    var entrypoint_index: ?u32 = null;
    for (mir_list.items) |mir| {
        if (mir.entrypoint) {
            entrypoint_index = mir.func_index;
            break;
        }
    }

    // Tree shake to find reachable functions
    const reachable = if (entrypoint_index) |idx|
        tree_shaker.shake(mir_list.items, idx)
    else
        tree_shaker.FuncSet{};

    // Combine name_mappings from reachable functions into one map
    var combined_name_map = std.AutoHashMapUnmanaged(u32, []const u8){};
    for (mir_list.items) |mir| {
        if (!reachable.contains(mir.func_index)) continue;
        for (mir.name_mappings) |mapping| {
            combined_name_map.put(clr_allocator.allocator(), mapping[0], mapping[1]) catch continue;
        }
    }

    // Combine field_mappings from reachable functions into one map
    var combined_field_map = FieldHashMap{};
    for (mir_list.items) |mir| {
        if (!reachable.contains(mir.func_index)) continue;
        for (mir.field_mappings) |mapping| {
            combined_field_map.put(clr_allocator.allocator(), mapping[0], mapping[1]) catch continue;
        }
    }

    // Write all reachable function stubs
    for (mir_list.items) |mir| {
        if (!reachable.contains(mir.func_index)) continue;
        file.writeAll(mir.text) catch return;
        file.writeAll("\n") catch return;
    }

    // Generate stubs for missing call targets
    const missing = tree_shaker.collectMissingTargets(mir_list.items, reachable);
    var it = missing.iterator();
    while (it.next()) |entry| {
        const stub = clr_codegen.generateStub(entry.key_ptr.*, entry.value_ptr.*);
        file.writeAll(stub) catch return;
        file.writeAll("\n") catch return;
    }

    // Emit getName function with combined mappings from reachable functions
    const getName_text = clr_codegen.emitGetName(&combined_name_map);
    file.writeAll(getName_text) catch return;

    // Emit getFieldId function with combined field mappings from reachable functions
    const getFieldId_text = clr_codegen.emitGetFieldId(&combined_field_map);
    file.writeAll(getFieldId_text) catch return;

    // Write epilogue (imports and main function)
    if (entrypoint_index) |idx| {
        const entry_text = clr_codegen.epilogue(idx);
        file.writeAll(entry_text) catch return;
    }

    file.setEndPos(file.getPos() catch return) catch return;
}

fn updateExports(_: c_anyopaque_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: usize) callconv(.c) void {}

fn deleteExport(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}
