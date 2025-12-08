const std = @import("std");
const compiler = @import("compiler");
const Zcu = compiler.Zcu;
const Air = compiler.Air;
const InternPool = compiler.InternPool;

// we can't use std.debug.print because there is some threadlocal stuff that isn't
// quite getting initialized correctly when we build the zig compiler in debug mode.
fn debugPrint(s: []const u8) void {
    _ = std.os.linux.write(2, s.ptr, s.len);
}

const c_anyopaque_t = [*c]const u8;

export fn generate(zcu: c_anyopaque_t, air: c_anyopaque_t, src_loc: c_anyopaque_t, func_index: u32) callconv(.c) c_anyopaque_t {
    const zcu_ptr: *const Zcu = @ptrCast(@alignCast(zcu));
    const air_ptr: *const Air = @ptrCast(@alignCast(air));
    const src_loc_ptr: *const Zcu.LazySrcLoc = @ptrCast(@alignCast(src_loc));
    const func_idx: InternPool.Index = @enumFromInt(func_index);
    generate_impl(zcu_ptr, air_ptr, src_loc_ptr.*, func_idx);
    return null;
}

pub fn generate_impl(zcu: *const Zcu, air: *const Air, src_loc: Zcu.LazySrcLoc, func_index: InternPool.Index) void {
    _ = air;
    _ = src_loc;
    const intern_pool = &zcu.intern_pool;
    const func = zcu.funcInfo(func_index);
    const nav = intern_pool.getNav(func.owner_nav);

    debugPrint("CLR: received function ");
    debugPrint(nav.fqn.toSlice(intern_pool));
    debugPrint("\n");
}
