const std = @import("std");
const clr_allocator = @import("allocator.zig");
const clr = @import("clr.zig");
const FuncMir = clr.FuncMir;
const CallTarget = clr.CallTarget;

pub const FuncSet = std.AutoHashMapUnmanaged(u32, void);

/// Build a set of all function indices reachable from the entrypoint
pub fn shake(mir_list: []*const FuncMir, entrypoint_index: u32) FuncSet {
    const allocator = clr_allocator.allocator();

    // Build lookup map: func_index -> FuncMir
    var func_map = std.AutoHashMapUnmanaged(u32, *const FuncMir){};
    for (mir_list) |mir| {
        func_map.put(allocator, mir.func_index, mir) catch continue;
    }

    // BFS from entrypoint
    var reachable = FuncSet{};
    var queue = std.ArrayListUnmanaged(u32){};

    queue.append(allocator, entrypoint_index) catch return reachable;
    reachable.put(allocator, entrypoint_index, {}) catch return reachable;

    while (queue.items.len > 0) {
        const func_index = queue.orderedRemove(0);
        const mir = func_map.get(func_index) orelse continue;

        for (mir.call_targets) |target| {
            if (!reachable.contains(target.index)) {
                if (func_map.contains(target.index)) {
                    reachable.put(allocator, target.index, {}) catch continue;
                    queue.append(allocator, target.index) catch continue;
                }
            }
        }
    }

    return reachable;
}

/// Collect all missing call targets (called but not defined)
pub fn collectMissingTargets(mir_list: []*const FuncMir, reachable: FuncSet) std.AutoHashMapUnmanaged(u32, u32) {
    const allocator = clr_allocator.allocator();

    // Map of missing func_index -> arity
    var missing = std.AutoHashMapUnmanaged(u32, u32){};

    for (mir_list) |mir| {
        if (!reachable.contains(mir.func_index)) continue;

        for (mir.call_targets) |target| {
            if (!reachable.contains(target.index)) {
                // This target is called but doesn't exist - record with its arity
                missing.put(allocator, target.index, target.arity) catch continue;
            }
        }
    }

    return missing;
}
