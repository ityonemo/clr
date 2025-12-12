const std = @import("std");
const compiler = @import("compiler");
const Zcu = compiler.Zcu;
const Air = compiler.Air;
const InternPool = compiler.InternPool;
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

const FuncMir = struct {
    func_index: u32,
    owner_nav: u32,
    text: []const u8,
    entrypoint: bool = false,
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

fn generate(_: c_anyopaque_t, pt_ptr: c_anyopaque_const_t, _: c_anyopaque_const_t, func_index: u32, air_ptr: c_anyopaque_const_t, _: c_anyopaque_const_t) callconv(.c) c_anyopaque_const_t {
    const pt: *const Zcu.PerThread = @ptrCast(@alignCast(pt_ptr));
    const func_air: *const Air = @ptrCast(@alignCast(air_ptr));

    const zcu = pt.zcu;
    const ip = &zcu.intern_pool;
    const func = zcu.funcInfo(@enumFromInt(func_index));
    const nav = ip.getNav(func.owner_nav);
    const fqn = nav.fqn.toSlice(ip);

    // Check if this is the main function (entrypoint)
    const root_name = zcu.root_mod.fully_qualified_name;
    var expected_main: [256]u8 = undefined;
    const expected_main_slice = std.fmt.bufPrint(&expected_main, "{s}.main", .{root_name}) catch return null;
    const is_entrypoint = std.mem.eql(u8, fqn, expected_main_slice);

    if (is_entrypoint) {
        debug.dumpAir(fqn, func_index, func_air);
    }

    // Get instruction tags and data from AIR
    const tags = func_air.instructions.items(.tag);
    const data = func_air.instructions.items(.data);

    // Generate Zig source for this function
    const text = clr_codegen.generateFunction(func_index, fqn, tags, data);

    const mir = clr_allocator.allocator().create(FuncMir) catch return null;
    mir.* = .{
        .func_index = func_index,
        .owner_nav = @intFromEnum(func.owner_nav),
        .text = text,
        .entrypoint = is_entrypoint,
    };
    return @ptrCast(mir);
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

    // Write all collected function stubs
    var entrypoint_index: ?u32 = null;
    for (mir_list.items) |mir| {
        file.writeAll(mir.text) catch return;
        file.writeAll("\n") catch return;
        if (mir.entrypoint) {
            entrypoint_index = mir.func_index;
        }
    }

    // Write epilogue (imports and main function)
    if (entrypoint_index) |idx| {
        const entry_text = clr_codegen.epilogue(idx) orelse return;
        file.writeAll(entry_text) catch return;
    }

    file.setEndPos(file.getPos() catch return) catch return;
}

fn updateExports(_: c_anyopaque_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: c_anyopaque_const_t, _: usize) callconv(.c) void {}

fn deleteExport(_: c_anyopaque_t, _: c_anyopaque_const_t, _: u32) callconv(.c) void {}
