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
    /// Root module name for filtering user code from stdlib
    root_name: []const u8 = "",
};

pub const FuncMir = struct {
    func_index: u32,
    owner_nav: u32,
    text: []const u8,
    call_targets: []const CallTarget,
    entrypoint: bool = false,
    name_mappings: []const NameMap,
    field_mappings: []const FieldMap,
    /// Return type string for entrypoints (e.g., ".{ .id = null, .ty = .{ .scalar = {} } }")
    /// Used to initialize the return slot with proper type structure
    return_type: ?[]const u8 = null,
};

// Collection of MIR structs
var mir_list: std.ArrayListUnmanaged(*const FuncMir) = .empty;

/// Source location for a global variable
pub const SourceLoc = struct {
    file: []const u8 = "",
    line: u32 = 0,
    column: u32 = 0,
};

/// Child relationship info for global variables - mirrors GlobalDef.ChildInfo
pub const ChildInfo = union(enum) {
    scalar: void,
    @"null": void, // optional that starts out null
    indirect: ?u32, // IP index for indirect targets, null if undefined
    struct_fields: []const ?u32, // IP indices for struct field globals
    union_field: struct {
        active_field_index: ?usize, // which field is active (null if whole union undefined)
        num_fields: usize, // total number of fields
        field_value_ip_idx: ?u32, // IP index for field value, null if undefined
    },
};

/// Information about a global variable for tracking
pub const GlobalInfo = struct {
    ip_idx: u32,
    type_str: []const u8, // Pre-formatted type string (wrapped in .undefined if global is undefined)
    loc: ?SourceLoc = null,
    children: ChildInfo = .{ .scalar = {} },
};

/// Registry of global variables encountered during codegen.
/// Maps ip_idx to GlobalInfo to avoid duplicates.
var global_registry: std.AutoHashMapUnmanaged(u32, GlobalInfo) = .empty;

/// Maps nav_idx to ip_idx for looking up parent struct globals from field pointers.
var nav_to_ip: std.AutoHashMapUnmanaged(u32, u32) = .empty;

/// Register a global variable. Returns the ip_idx for reference.
/// The type_str should be wrapped in .{ .ty = .{ .undefined = &inner } } if the global is undefined.
/// If nav_idx is provided, also records the nav→ip mapping for field pointer lookups.
pub fn registerGlobal(ip_idx: u32, type_str: []const u8, children: ChildInfo) u32 {
    if (global_registry.get(ip_idx) == null) {
        global_registry.put(clr_allocator.allocator(), ip_idx, .{
            .ip_idx = ip_idx,
            .type_str = type_str,
            .children = children,
        }) catch {};
    }
    return ip_idx;
}

/// Register a global with nav→ip mapping for parent struct lookup.
pub fn registerGlobalWithNav(ip_idx: u32, nav_idx: u32, type_str: []const u8, children: ChildInfo) u32 {
    nav_to_ip.put(clr_allocator.allocator(), nav_idx, ip_idx) catch {};
    return registerGlobal(ip_idx, type_str, children);
}

/// Register a struct field pointer and update the parent struct's struct_fields.
/// nav_idx is the nav of the parent struct, field_index is which field, field_ip_idx is this field pointer's IP index.
pub fn registerFieldPointer(nav_idx: u32, field_index: usize, num_fields: usize, field_ip_idx: u32, field_type_str: []const u8) void {
    // Register the field pointer itself
    _ = registerGlobal(field_ip_idx, field_type_str, .{ .scalar = {} });

    // Find the parent struct by nav_idx
    const parent_ip_idx = nav_to_ip.get(nav_idx) orelse return;
    const parent = global_registry.getPtr(parent_ip_idx) orelse return;

    // Get or create the struct_fields array
    const struct_fields: []?u32 = switch (parent.children) {
        .struct_fields => |existing| @constCast(existing),
        else => blk: {
            // First field pointer for this struct - allocate the array
            const fields = clr_allocator.allocator().alloc(?u32, num_fields) catch return;
            @memset(fields, null);
            parent.children = .{ .struct_fields = fields };
            break :blk fields;
        },
    };

    // Update the field at this index
    if (field_index < struct_fields.len) {
        struct_fields[field_index] = field_ip_idx;
    }
}

/// Get all registered globals as an iterator
pub fn getGlobalsIterator() std.AutoHashMapUnmanaged(u32, GlobalInfo).ValueIterator {
    return global_registry.valueIterator();
}

/// Get the number of registered globals
pub fn getGlobalsCount() usize {
    return global_registry.count();
}

/// Update location info for all registered globals that don't have it yet.
/// Called after generateFunction with access to Zcu.
/// Note: IP indices are pointers; we extract nav from base_addr to get location.
pub fn updateGlobalLocations(zcu: *Zcu) void {
    const ip = &zcu.intern_pool;
    var it = global_registry.iterator();
    while (it.next()) |entry| {
        // Skip if already has location
        if (entry.value_ptr.loc != null) continue;

        const ip_idx: InternPool.Index = @enumFromInt(entry.key_ptr.*);

        // Get the pointer key to extract the nav
        const ptr_key = ip.indexToKey(ip_idx);
        if (ptr_key != .ptr) continue;
        const ptr = ptr_key.ptr;
        if (ptr.base_addr != .nav) continue;
        const nav_idx = ptr.base_addr.nav;

        // Get source location info from the nav
        const inst_info = ip.getNav(nav_idx).srcInst(ip).resolveFull(ip) orelse continue;
        const file_idx = inst_info.file;
        const file = zcu.fileByIndex(file_idx);
        const zir = file.zir orelse continue;
        const decl = zir.getDeclaration(inst_info.inst);

        // Get file path and dupe to persistent allocator
        const file_path = clr_allocator.allocator().dupe(u8, file.path.sub_path) catch continue;

        entry.value_ptr.loc = .{
            .file = file_path,
            // Add 1 to convert from 0-indexed to 1-indexed (consistent with dbg_stmt handling)
            .line = decl.src_line + 1,
            .column = decl.src_column + 1,
        };
    }
}

export fn init(avt: *const clr_allocator.AllocatorVTable) ?*const u8 {
    clr_allocator.init(avt);
    mir_list = .empty;
    global_registry = .empty;
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
        .root_name = root_name,
    };

    // Generate Zig source for this function
    const text = clr_codegen.generateFunction(func_index, fqn, &info, base_line, file_path);

    // Update location info for any globals that were registered during codegen
    updateGlobalLocations(zcu);

    // Convert hash maps to slices for storage on FuncMir
    const name_mappings = convertNameMapToSlice(&name_map);
    const field_mappings = convertFieldMapToSlice(&field_map);

    // Extract call targets from AIR (skips debug.* calls)
    const call_targets = clr_codegen.extractCallTargets(clr_allocator.allocator(), ip, tags, data, extra);

    // Extract return type for entrypoints (to initialize return slot with proper type)
    const return_type: ?[]const u8 = if (is_entrypoint) blk: {
        const ret_type_str = clr_codegen.extractFunctionReturnType(&name_map, &field_map, arena.allocator(), ip, @enumFromInt(func_index));
        // Dupe to persistent allocator since arena will be freed
        break :blk clr_allocator.allocator().dupe(u8, ret_type_str) catch null;
    } else null;

    const mir = clr_allocator.allocator().create(FuncMir) catch return null;
    mir.* = .{
        .func_index = func_index,
        .owner_nav = @intFromEnum(func.owner_nav),
        .text = text,
        .call_targets = call_targets,
        .entrypoint = is_entrypoint,
        .name_mappings = name_mappings,
        .field_mappings = field_mappings,
        .return_type = return_type,
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

fn updateNav(_: c_anyopaque_t, pt_ptr: c_anyopaque_const_t, nav_index_raw: u32) callconv(.c) void {
    const pt: *const Zcu.PerThread = @ptrCast(@alignCast(pt_ptr));
    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const nav_index: InternPool.Nav.Index = @enumFromInt(nav_index_raw);
    const nav = ip.getNav(nav_index);

    // Only process fully resolved navs
    const resolved = switch (nav.status) {
        .fully_resolved => |r| r,
        else => return,
    };

    // Skip functions - we only want global variables
    const type_tag = ip.zigTypeTag(ip.typeOf(resolved.val));
    if (type_tag == .@"fn") return;

    // Get root module name to filter user code from stdlib
    const root_name = zcu.root_mod.fully_qualified_name;
    const fqn = nav.fqn.toSlice(ip);

    // Skip non-user globals (stdlib, compiler_rt, etc)
    if (!std.mem.startsWith(u8, fqn, root_name)) return;

    // Note: Don't skip if already registered - updateNav has accurate type info
    // (including null/undefined status) that codegen may not have had access to.

    // Create arena for type string generation
    var arena = clr_allocator.newArena();
    defer arena.deinit();

    // Get the type of this global
    const nav_type = nav.typeOf(ip);

    // Get the actual init value - for variables, we need to look inside
    const val_key = ip.indexToKey(resolved.val);
    const init_val: InternPool.Index = switch (val_key) {
        .variable => |v| v.init,
        else => resolved.val,
    };

    // Check if value is undefined or null
    const init_key = if (init_val != .none) ip.indexToKey(init_val) else val_key;
    const is_undefined = (init_key == .undef) or (init_val == .none);
    const is_null = switch (init_key) {
        .opt => |opt| opt.val == .none,
        else => false,
    };

    // Generate type string (with per-field undefined tracking for struct aggregates)
    const type_str = clr_codegen.typeToStringForGlobalWithInit(arena.allocator(), ip, nav_type, init_val);

    // Wrap in .undefined if the global is undefined, or .null if it's a null optional
    // For .null, we need the INNER type (not the optional wrapper) since .null implies optional
    const final_type_str = if (is_undefined)
        clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .ty = .{{ .undefined = &{s} }} }}", .{type_str}, null)
    else if (is_null) blk: {
        // Extract the child type from the optional
        const type_key = ip.indexToKey(nav_type);
        const inner_type = if (type_key == .opt_type) type_key.opt_type else nav_type;
        const inner_type_str = clr_codegen.typeToStringForGlobal(arena.allocator(), ip, inner_type);
        break :blk clr_allocator.allocPrint(clr_allocator.allocator(), ".{{ .ty = .{{ .null = &{s} }} }}", .{inner_type_str}, null);
    } else
        clr_allocator.allocator().dupe(u8, type_str) catch return;

    // Detect union type and extract active field info
    const children: ChildInfo = blk: {
        const type_key = ip.indexToKey(nav_type);
        if (type_key != .union_type) break :blk .{ .scalar = {} };

        // Get the number of fields in this union
        const loaded_union = ip.loadUnionType(nav_type);
        const num_fields = loaded_union.field_types.len;

        // If undefined, no active field
        if (is_undefined) break :blk .{ .union_field = .{ .active_field_index = null, .num_fields = num_fields, .field_value_ip_idx = null } };

        // Extract active field index from union init value
        if (init_key == .un) {
            const un = init_key.un;
            // Get the tag to find the active field index
            if (un.tag != .none) {
                const tag_key = ip.indexToKey(un.tag);
                if (tag_key == .enum_tag) {
                    // The int field is an Index that holds the integer tag value
                    const int_key = ip.indexToKey(tag_key.enum_tag.int);
                    if (int_key == .int) {
                        // Extract the field index from the integer value
                        const field_index: usize = @intCast(int_key.int.storage.u64);
                        // Check if field value is undefined
                        const field_val_key = ip.indexToKey(un.val);
                        const field_value_ip_idx: ?u32 = if (field_val_key == .undef) null else @intFromEnum(un.val);
                        break :blk .{ .union_field = .{ .active_field_index = field_index, .num_fields = num_fields, .field_value_ip_idx = field_value_ip_idx } };
                    }
                }
            }
        }

        // Union type but couldn't determine active field
        break :blk .{ .union_field = .{ .active_field_index = null, .num_fields = num_fields, .field_value_ip_idx = null } };
    };

    // Get source location info
    var file_path: []const u8 = "";
    var line: u32 = 0;
    var column: u32 = 0;
    if (nav.srcInst(ip).resolveFull(ip)) |inst_info| {
        const file_idx = inst_info.file;
        const file = zcu.fileByIndex(file_idx);
        if (file.zir) |zir| {
            const decl = zir.getDeclaration(inst_info.inst);
            file_path = clr_allocator.allocator().dupe(u8, file.path.sub_path) catch "";
            line = decl.src_line + 1;
            column = decl.src_column + 1;
        }
    }

    // Register the global, using detected children (or preserving existing struct_fields/indirect)
    // Look up the actual IP index if it was already registered by codegen
    const actual_ip_idx = nav_to_ip.get(nav_index_raw) orelse nav_index_raw;

    const final_children = blk: {
        // If we detected union_field, use them
        if (children == .union_field) break :blk children;
        // Otherwise preserve existing struct_fields/indirect if already set
        if (global_registry.get(actual_ip_idx)) |existing| {
            if (existing.children != .scalar) break :blk existing.children;
        }
        break :blk children;
    };
    global_registry.put(clr_allocator.allocator(), actual_ip_idx, .{
        .ip_idx = actual_ip_idx,
        .type_str = final_type_str,
        .loc = .{ .file = file_path, .line = line, .column = column },
        .children = final_children,
    }) catch {};
}

fn updateLineNumber(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}

fn flush(lf_ptr: c_anyopaque_t, _: c_anyopaque_const_t, _: u32, _: c_anyopaque_const_t) callconv(.c) void {
    const lf: *link.File = @ptrCast(@alignCast(lf_ptr));
    const file = lf.file orelse return;

    file.seekTo(0) catch return;

    // Find entrypoint first
    var entrypoint_index: ?u32 = null;
    var entrypoint_return_type: ?[]const u8 = null;
    for (mir_list.items) |mir| {
        if (mir.entrypoint) {
            entrypoint_index = mir.func_index;
            entrypoint_return_type = mir.return_type;
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
        const entry_text = clr_codegen.epilogue(idx, entrypoint_return_type);
        file.writeAll(entry_text) catch return;
    }

    file.setEndPos(file.getPos() catch return) catch return;
}

fn updateExports(_: c_anyopaque_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: usize) callconv(.c) void {}

fn deleteExport(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}
