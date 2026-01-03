const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Analyte = @import("../Analyte.zig");
const Gid = Refinements.Gid;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// =========================================================================
// State types
// =========================================================================

pub const Stack = struct {
    meta: Meta,
    root_gid: ?Gid, // null = root, else parent container gid
    name: Name = .{ .other = {} },

    pub const Name = union(enum) {
        variable: u32, // Name ID (resolved via ctx.getName at error time)
        parameter: u32, // Name ID (resolved via ctx.getName at error time)
        other: void,
    };
};

pub const Free = struct {
    meta: Meta,
    name_at_free: ?[]const u8 = null, // Full path name (arena-allocated at store time)
};

pub const Allocated = struct {
    meta: Meta,
    root_gid: ?Gid, // null = root, else parent container gid
    freed: ?Free = null, // null = still allocated, has value = freed
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages
    name_at_alloc: ?[]const u8 = null, // Full path name when allocated (e.g., "container.ptr")
    returned: bool = false, // true if this allocation was returned to caller (not a local leak)
};

pub const MemorySafety = union(enum) {
    stack: Stack,
    allocated: Allocated,
    unset: void,

    pub fn alloc(state: State, index: usize, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, set memory_safety on pointer and pointee
        const ptr_idx = state.results[index].refinement.?;
        const ptr = &state.refinements.at(ptr_idx).pointer;
        const pointee_idx = ptr.to;

        // Set memory_safety on the pointer itself
        ptr.analyte.memory_safety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };

        // Set memory_safety recursively on the pointee
        setStackRecursive(state.refinements, pointee_idx, state.ctx.meta, null);
    }

    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        // ptr is null for stores to interned/global locations - no memory safety tracking needed
        const ptr = params.ptr orelse return;
        const src = switch (params.src) {
            .inst => |idx| idx,
            // comptime/global values don't have memory safety tracking - skip
            .interned, .other => return,
        };

        // When storing a pointer with allocation tracking, set name_at_alloc on the pointee
        // This captures the access path (e.g., "container.ptr") for error messages
        const src_refinement_idx = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_refinement_idx);
        if (src_refinement.* == .pointer) {
            // Get the pointee and check if it has allocation tracking
            const pointee_idx = src_refinement.pointer.to;
            const pointee = refinements.at(pointee_idx);
            const pointee_analyte = getAnalytePtr(pointee);
            if (pointee_analyte.memory_safety) |*ms| {
                if (ms.* == .allocated) {
                    // Set name_at_alloc if not already set
                    if (ms.allocated.name_at_alloc == null) {
                        ms.allocated.name_at_alloc = ctx.buildPathName(results, refinements, ptr);
                    }
                }
            }
        }

        // If storing from a parameter, propagate the parameter name_id and location to the destination's stack_ptr
        // Get name_id from the arg tag (name_id on Inst is only set by dbg_var_ptr)
        const src_tag = results[src].inst_tag orelse return;
        const param_name_id = switch (src_tag) {
            .arg => |a| a.name_id,
            else => return,
        };
        // Get the target pointer refinement (created by arg for pointer parameters)
        const tgt_refinement_idx = results[ptr].refinement orelse return;
        if (refinements.at(tgt_refinement_idx).* != .pointer) return;
        const tgt_ptr = &refinements.at(tgt_refinement_idx).pointer;

        if (tgt_ptr.analyte.memory_safety) |*ms| {
            if (ms.* == .stack) {
                ms.stack.name = .{ .parameter = param_name_id };
                ms.stack.meta = .{
                    .function = ctx.meta.function,
                    .file = ctx.meta.file,
                    .line = ctx.base_line + 1,
                    .column = null,
                };
            }
        }
    }

    /// Retroactively set variable name on stack for escape detection messages.
    /// The name_id is already set on the instruction by DbgVarPtr.apply().
    /// With new architecture, memory_safety is on the POINTEE, so we follow the pointer.
    pub fn dbg_var_ptr(state: State, index: usize, params: tag.DbgVarPtrParams) !void {
        _ = index;
        const inst = params.ptr orelse return;
        const ptr_idx = state.results[inst].refinement orelse return;
        const ptr_ref = state.refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        // Follow the pointer to the pointee where memory_safety is stored
        const pointee_idx = ptr_ref.pointer.to;
        const pointee = state.refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        if (pointee_analyte.memory_safety) |*ms| {
            if (ms.* == .stack) {
                if (ms.stack.name == .other) {
                    ms.stack.name = .{ .variable = params.name_id };
                }
            }
        }
    }

    /// For interned args, no memory safety tracking needed (constants have no allocation state).
    /// For eidx args, memory safety state was already copied from caller.
    /// TODO: See LIMITATIONS.md - interned pointer args may need special handling.
    pub fn arg(state: State, index: usize, params: tag.Arg) !void {
        _ = state;
        _ = index;
        _ = params;
        // Nothing to do - interned values have no memory safety state,
        // and eidx values already have their state copied from caller.
    }

    /// struct_field_ptr creates a pointer to a field of a struct/union.
    /// Copy the field's memory_safety to the pointer so it knows what kind of memory it points to.
    /// This preserves root_gid which is needed to detect "free of field pointer" errors.
    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        const refinements = state.refinements;
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        const pointee_idx = ptr.to;
        const pointee = refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // If pointee has memory_safety, copy it to the pointer
        if (pointee_analyte.memory_safety) |ms| {
            ptr.analyte.memory_safety = ms;
            return;
        }

        // Pointee has no memory_safety - this can happen for newly created union fields.
        // Propagate from the base container and set on both pointee and pointer.
        const base = params.base orelse return;
        const base_idx = state.results[base].refinement orelse return;
        const base_ref = refinements.at(base_idx);
        if (base_ref.* != .pointer) return;

        const container_idx = base_ref.pointer.to;
        const container = refinements.at(container_idx);
        const container_analyte = getAnalytePtr(container);
        const container_ms = container_analyte.memory_safety orelse return;

        // Create memory_safety for the field with root_gid pointing to container
        const container_gid = container.getGid();
        const field_ms: MemorySafety = switch (container_ms) {
            .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = container_gid } },
            .allocated => |a| .{ .allocated = .{
                .meta = a.meta,
                .freed = a.freed,
                .type_id = a.type_id,
                .root_gid = container_gid,
            } },
            .unset => .{ .unset = {} },
        };

        // Set on both pointee and pointer
        pointee_analyte.memory_safety = field_ms;
        ptr.analyte.memory_safety = field_ms;
    }

    /// field_parent_ptr recovers the parent container pointer from a field pointer.
    /// We use the stored root_gid to reconnect to the original container entity.
    pub fn field_parent_ptr(state: State, index: usize, params: tag.FieldParentPtr) !void {
        const refinements = state.refinements;

        const field_ptr = params.field_ptr orelse return; // Can't track interned
        const ptr_idx = state.results[field_ptr].refinement orelse return;
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        // Get parent GID from the field pointer's memory_safety.root_gid
        const ms = ptr_ref.pointer.analyte.memory_safety orelse return;
        const parent_gid: ?Gid = switch (ms) {
            .stack => |s| s.root_gid,
            .allocated => |a| a.root_gid,
            .unset => return, // Unset pointer, can't recover parent
        };
        const root_gid = parent_gid orelse return; // null means this IS the root, no parent
        const parent_eidx = refinements.findByGid(root_gid) orelse return;

        // Update result to point to original parent container
        const result_idx = state.results[index].refinement orelse return;
        const result_ptr = &refinements.at(result_idx).pointer;
        result_ptr.to = parent_eidx;

        // Copy parent's memory_safety to the result pointer
        // This is now pointing at the root, so root_gid = null
        const parent_analyte = getAnalytePtr(refinements.at(parent_eidx));
        const parent_ms = parent_analyte.memory_safety orelse return;
        result_ptr.analyte.memory_safety = switch (parent_ms) {
            .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = null } },
            .allocated => |a| .{ .allocated = .{
                .meta = a.meta,
                .freed = a.freed,
                .type_id = a.type_id,
                .root_gid = null,
            } },
            .unset => .{ .unset = {} },
        };
    }

    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const src = switch (params.src) {
            .inst => |idx| idx,
            // comptime/global values don't have memory safety tracking - skip
            .interned, .other => return,
        };
        // refinement is null for uninitialized instructions - skip (would be caught by undefined analysis)
        const src_idx = results[src].refinement orelse return;

        // Recursively check for allocations to mark as returned
        try markAllocationsAsReturned(refinements, src_idx, ctx);
    }

    /// Recursively mark all allocations in a refinement tree as returned (not leaks).
    /// This handles pointers, optionals containing pointers, unions containing pointers, and structs.
    fn markAllocationsAsReturned(refinements: *Refinements, idx: Gid, ctx: *Context) !void {
        const refinement = refinements.at(idx);
        switch (refinement.*) {
            .pointer => |*p| {
                // Check pointer's memory_safety for stack pointer escape
                if (p.analyte.memory_safety) |ms| {
                    if (ms == .stack) {
                        const sp = ms.stack;
                        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                        if (std.mem.eql(u8, sp.meta.function, func_name)) {
                            return reportStackEscape(ms, ctx);
                        }
                    }
                }

                // Check pointee's memory_safety for allocation state (heap allocation tracking)
                const pointee_idx = p.to;
                const pointee = refinements.at(pointee_idx);
                const pointee_analyte = getAnalytePtr(pointee);
                if (pointee_analyte.memory_safety) |*ms| {
                    if (ms.* == .allocated) {
                        // Mark the allocation as returned - not a leak in this function
                        ms.allocated.returned = true;
                    }
                }
            },
            .optional => |o| {
                // Check inner for pointers
                try markAllocationsAsReturned(refinements, o.to, ctx);
            },
            .errorunion => |e| {
                // Check payload for pointers
                try markAllocationsAsReturned(refinements, e.to, ctx);
            },
            .@"struct" => |s| {
                // Check all fields for pointers
                for (s.fields) |field_idx| {
                    try markAllocationsAsReturned(refinements, field_idx, ctx);
                }
            },
            .@"union" => |u| {
                // Check all fields for pointers
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        try markAllocationsAsReturned(refinements, field_idx, ctx);
                    }
                }
            },
            .scalar, .void, .noreturn, .unimplemented, .retval_future, .region => {},
        }
    }

    /// Clear the `returned` flag on allocations in a received return value.
    /// Called by the caller after receiving a value from a function call.
    /// This transfers ownership: the callee marked it as "returned" (its responsibility ends),
    /// but the caller now owns it and must either free it or return it.
    pub fn clearAllocationsReturned(refinements: *Refinements, idx: Gid) void {
        const refinement = refinements.at(idx);
        switch (refinement.*) {
            .pointer => |*p| {
                // Check pointee's memory_safety for allocation state
                const pointee_idx = p.to;
                const pointee = refinements.at(pointee_idx);
                const pointee_analyte = getAnalytePtr(pointee);
                if (pointee_analyte.memory_safety) |*ms| {
                    if (ms.* == .allocated) {
                        // Clear returned flag - caller now owns this allocation
                        ms.allocated.returned = false;
                    }
                }
            },
            .optional => |o| {
                clearAllocationsReturned(refinements, o.to);
            },
            .errorunion => |e| {
                clearAllocationsReturned(refinements, e.to);
            },
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    clearAllocationsReturned(refinements, field_idx);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        clearAllocationsReturned(refinements, field_idx);
                    }
                }
            },
            .scalar, .void, .noreturn, .unimplemented, .retval_future, .region => {},
        }
    }

    /// Check ret_load for stack pointer escapes and mark allocations as returned.
    /// ret_load returns a value through ret_ptr storage - used for large returns (structs, unions).
    pub fn ret_load(state: State, index: usize, params: tag.RetLoad) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Get the ret_ptr pointer and its pointee (the struct/union being returned)
        const ptr_refinement_idx = results[params.ptr].refinement orelse return;
        const ptr_refinement = refinements.at(ptr_refinement_idx);
        if (ptr_refinement.* != .pointer) return;

        const pointee_idx = ptr_refinement.pointer.to;
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];

        // Check for stack pointers escaping via struct/union fields
        try checkStackEscapeRecursive(refinements, pointee_idx, ctx, func_name);

        // Mark any heap allocations in the returned value as "returned" (not leaks)
        try markAllocationsAsReturned(refinements, pointee_idx, ctx);
    }

    /// Recursively check refinement tree for escaping stack pointers.
    /// Only checks for stack_ptr escapes - does NOT modify allocation state.
    fn checkStackEscapeRecursive(refinements: *Refinements, idx: Gid, ctx: *Context, func_name: []const u8) !void {
        switch (refinements.at(idx).*) {
            .pointer => |p| {
                const ms = p.analyte.memory_safety orelse return;
                switch (ms) {
                    .stack => |sp| {
                        if (std.mem.eql(u8, sp.meta.function, func_name)) {
                            return reportStackEscape(ms, ctx);
                        }
                    },
                    .allocated, .unset => {},
                }
            },
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    try checkStackEscapeRecursive(refinements, field_idx, ctx, func_name);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        try checkStackEscapeRecursive(refinements, field_idx, ctx, func_name);
                    }
                }
            },
            .optional => |o| try checkStackEscapeRecursive(refinements, o.to, ctx, func_name),
            .errorunion => |e| try checkStackEscapeRecursive(refinements, e.to, ctx, func_name),
            .scalar, .void, .noreturn, .unimplemented, .retval_future, .region => {},
        }
    }

    /// Called on function close to check for memory leaks and stack pointer escapes.
    /// With global refinements, args share entities directly with caller.
    /// Stack pointer escapes through args are detected by checking arg pointees.
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
        // Check for stack pointer escapes via pointer arguments
        // If a pointer arg's pointee contains a stack pointer from this function,
        // it escapes to the caller via the shared global refinements table
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        for (results) |inst| {
            // Only check args (instructions with .arg tag)
            const any_tag = inst.inst_tag orelse continue;
            if (any_tag != .arg) continue;
            const idx = inst.refinement orelse continue;
            const refinement = refinements.at(idx);

            // Only check pointer args - scalar args are copied by value
            if (refinement.* != .pointer) continue;

            // Check the pointee for stack pointers
            try checkStackEscapeRecursive(refinements, refinement.pointer.to, ctx, func_name);
        }

        // Check for memory leaks - allocation state is on the POINTEE
        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            const refinement = refinements.at(idx);
            if (refinement.* != .pointer) continue;

            // Get the pointee entity via ptr.to
            const pointee_idx = refinement.pointer.to;
            const pointee = refinements.at(pointee_idx);
            const pointee_analyte = getAnalytePtr(pointee);

            // Check pointee's memory_safety for allocation state
            const ms = pointee_analyte.memory_safety orelse continue;
            if (ms != .allocated) continue;

            const allocation = ms.allocated;
            // Skip if freed or returned to caller
            if (allocation.freed == null and !allocation.returned) {
                return reportMemoryLeak(ctx, allocation);
            }
        }
    }

    // =========================================================================
    // Allocation tracking (use-after-free, double-free, memory leak detection)
    // =========================================================================

    /// Handle allocator.create() - marks pointee as heap allocation.
    /// Allocation state is tracked on the POINTEE, not the pointer.
    /// This way, semideep copies of the pointer share the same pointee
    /// and see the same allocation state (freed, etc.).
    pub fn alloc_create(state: State, index: usize, params: tag.AllocCreate) !void {
        // Result is errorunion -> ptr -> pointee
        const eu_idx = state.results[index].refinement.?;
        const eu = &state.refinements.at(eu_idx).errorunion;
        const ptr_ref = eu.to;
        const ptr = &state.refinements.at(ptr_ref).pointer;
        const pointee_ref = ptr.to;

        const alloc_base: AllocatedBase = .{
            .meta = state.ctx.meta,
            .type_id = params.type_id,
            .root_gid = null, // This is the root allocation
        };

        // Set memory_safety on errorunion
        eu.analyte.memory_safety = .{ .allocated = .{
            .meta = alloc_base.meta,
            .type_id = alloc_base.type_id,
            .root_gid = null,
        } };

        // Set memory_safety on pointer
        ptr.analyte.memory_safety = .{ .allocated = .{
            .meta = alloc_base.meta,
            .type_id = alloc_base.type_id,
            .root_gid = null,
        } };

        // Set allocation state recursively on the pointee
        setAllocatedRecursive(state.refinements, pointee_ref, alloc_base, null);
    }

    /// Get a mutable pointer to the Analyte for any refinement type that has one
    fn getAnalytePtr(ref: *Refinements.Refinement) *Analyte {
        return switch (ref.*) {
            .scalar => |*s| &s.analyte,
            .pointer => |*p| &p.analyte,
            .optional => |*o| &o.analyte,
            .errorunion => |*e| &e.analyte,
            .@"struct" => |*st| &st.analyte,
            .@"union" => |*u| &u.analyte,
            .region => |*r| &r.analyte,
            else => @panic("refinement type does not have analyte"),
        };
    }

    // =========================================================================
    // Recursive memory_safety propagation helpers
    // =========================================================================

    /// Recursively set .stack memory_safety on all members of a refinement.
    /// Root gets root_gid = null (can free directly), children get root_gid = root's gid.
    /// Pointers STOP recursion (they point to separate memory).
    fn setStackRecursive(refinements: *Refinements, idx: Gid, meta: Meta, root_gid: ?Gid) void {
        const ref = refinements.at(idx);
        const ms: MemorySafety = .{ .stack = .{
            .meta = meta,
            .root_gid = root_gid,
        } };
        // Children point to actual root (null for root, else the root's gid)
        const child_root = root_gid orelse ref.getGid();

        switch (ref.*) {
            .scalar => |*s| s.analyte.memory_safety = ms,
            .@"struct" => |*st| {
                st.analyte.memory_safety = ms;
                for (st.fields) |field_idx| {
                    setStackRecursive(refinements, field_idx, meta, child_root);
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = ms;
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setStackRecursive(refinements, field_idx, meta, child_root);
                    }
                }
            },
            .optional => |*o| {
                o.analyte.memory_safety = ms;
                setStackRecursive(refinements, o.to, meta, child_root);
            },
            .errorunion => |*e| {
                e.analyte.memory_safety = ms;
                setStackRecursive(refinements, e.to, meta, child_root);
            },
            .pointer => |*p| {
                // Set memory_safety on the pointer field itself (it's part of the struct)
                // but don't recurse into what it points to (that's separate memory)
                p.analyte.memory_safety = ms;
            },
            else => {},
        }
    }

    /// Base allocation info for setAllocatedRecursive (without root_gid, which is computed).
    const AllocatedBase = struct {
        meta: Meta,
        type_id: u32,
        root_gid: ?Gid, // Initial root_gid for the root node
    };

    /// Recursively set .allocated memory_safety on all members of a refinement.
    /// Root gets root_gid from base (typically null), children get root_gid = root's gid.
    /// Pointers STOP recursion (they point to separate memory).
    fn setAllocatedRecursive(refinements: *Refinements, idx: Gid, base: AllocatedBase, root_gid: ?Gid) void {
        const ref = refinements.at(idx);
        const ms: MemorySafety = .{ .allocated = .{
            .meta = base.meta,
            .type_id = base.type_id,
            .root_gid = root_gid,
        } };
        // Children point to actual root (null for root, else the root's gid)
        const child_root = root_gid orelse ref.getGid();

        switch (ref.*) {
            .scalar => |*s| s.analyte.memory_safety = ms,
            .@"struct" => |*st| {
                st.analyte.memory_safety = ms;
                for (st.fields) |field_idx| {
                    setAllocatedRecursive(refinements, field_idx, base, child_root);
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = ms;
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setAllocatedRecursive(refinements, field_idx, base, child_root);
                    }
                }
            },
            .optional => |*o| {
                o.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, o.to, base, child_root);
            },
            .errorunion => |*e| {
                e.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, e.to, base, child_root);
            },
            .pointer => |*p| {
                // Set memory_safety on the pointer field itself (it's part of the struct)
                // but don't recurse into what it points to (that's separate memory)
                p.analyte.memory_safety = ms;
            },
            else => {},
        }
    }

    /// Recursively set .freed on all entities with .allocated memory_safety.
    /// This propagates the freed state to all struct/union fields.
    fn setFreedRecursive(refinements: *Refinements, idx: Gid, free_meta: Free) void {
        const ref = refinements.at(idx);
        const analyte = getAnalytePtr(ref);

        // Set freed on this entity if it has .allocated
        if (analyte.memory_safety) |*ms| {
            if (ms.* == .allocated) {
                ms.allocated.freed = free_meta;
            }
        }

        // Recurse into children (struct fields, union variants, optional/errorunion payload)
        switch (ref.*) {
            .@"struct" => |st| {
                for (st.fields) |field_idx| {
                    setFreedRecursive(refinements, field_idx, free_meta);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setFreedRecursive(refinements, field_idx, free_meta);
                    }
                }
            },
            .optional => |o| setFreedRecursive(refinements, o.to, free_meta),
            .errorunion => |e| setFreedRecursive(refinements, e.to, free_meta),
            else => {},
        }
    }

    /// Handle allocator.destroy() - marks as freed, detects double-free and mismatched allocator.
    /// Looks up the pointee via ptr.to to find the allocation state.
    pub fn alloc_destroy(state: State, index: usize, params: tag.AllocDestroy) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const ptr = params.ptr;

        const ptr_idx = results[ptr].refinement orelse @panic("alloc_destroy: inst has no refinement");
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) @panic("alloc_destroy: expected pointer refinement");

        // Get the pointee entity via ptr.to
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee = refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Get memory_safety from pointee (new architecture: memory_safety is on pointee)
        const ms = &(pointee_analyte.memory_safety orelse
            @panic("alloc_destroy: pointee has no memory_safety"));

        switch (ms.*) {
            .stack => |sp| {
                // Check if this is a field pointer (root_gid != null means it's a field)
                if (sp.root_gid != null) {
                    return reportFreeFieldPointerStack(ctx, sp);
                }
                return reportFreeStackMemory(ctx, sp);
            },
            .unset => @panic("alloc_destroy: pointee memory_safety is unset"),
            .allocated => {},
        }

        const a = ms.allocated;

        // Check if this is a field pointer (root_gid != null means it's a field, can't free directly)
        if (a.root_gid != null) {
            return reportFreeFieldPointer(ctx, a);
        }

        if (a.freed) |previous_free| {
            return reportDoubleFree(ctx, a, previous_free);
        }
        if (a.type_id != params.type_id) {
            return reportMismatchedAllocator(ctx, a, params.type_id);
        }

        // Mark as freed using setFreedRecursive to propagate to all fields
        const free_meta: Free = .{
            .meta = ctx.meta,
            .name_at_free = ctx.buildPathName(results, refinements, ptr),
        };
        setFreedRecursive(refinements, pointee_idx, free_meta);
    }

    /// Handle load - detect use-after-free.
    /// Checks the POINTEE's allocation state (via ptr.to) for freed status.
    pub fn load(state: State, index: usize, params: tag.Load) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Set result pointer's memory_safety to .stack (one deep only)
        // The loaded pointer is a value copy living on the stack
        const result_idx = results[index].refinement orelse return;
        const result_ref = refinements.at(result_idx);
        if (result_ref.* == .pointer) {
            result_ref.pointer.analyte.memory_safety = .{ .stack = .{
                .meta = ctx.meta,
                .root_gid = null,
            } };
        }

        // ptr is null for interned/global loads - no memory safety tracking needed
        const ptr = params.ptr orelse return;
        // refinement may be null for uninitialized instructions - skip
        const ptr_idx = results[ptr].refinement orelse return;
        const ptr_refinement = refinements.at(ptr_idx);

        // Loading through a non-pointer is a bug - we should only load through pointers
        if (ptr_refinement.* != .pointer) {
            std.debug.panic("memory_safety.load: expected pointer, got {s}", .{@tagName(ptr_refinement.*)});
        }

        // Get the pointee entity via ptr.to
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee = refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // memory_safety may be null for stack allocations or untracked pointers
        const ms = pointee_analyte.memory_safety orelse return;
        // Only check use-after-free for heap allocations
        if (ms != .allocated) return;

        if (ms.allocated.freed) |free_site| {
            return reportUseAfterFree(ctx, ms.allocated, free_site);
        }
    }

    // =========================================================================
    // Error reporting
    // =========================================================================

    fn reportStackEscape(ms: MemorySafety, ctx: *Context) anyerror {
        const sp = ms.stack;
        try ctx.meta.print(ctx.writer, "stack pointer escape in ", .{});
        switch (sp.name) {
            .variable => |name_id| {
                const name = ctx.getName(name_id);
                try sp.meta.print(ctx.writer, "pointer was for local variable '{s}' in ", .{name});
            },
            .parameter => |name_id| {
                const name = ctx.getName(name_id);
                if (name.len > 0) {
                    try sp.meta.print(ctx.writer, "pointer was for parameter '{s}' created in ", .{name});
                } else {
                    try sp.meta.print(ctx.writer, "pointer was for parameter created in ", .{});
                }
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer was for stack memory created in ", .{});
            },
        }
        return error.StackPointerEscape;
    }

    /// Format name prefix for allocation error messages.
    /// With name: "'container.ptr' "
    /// Without name: ""
    fn formatNamePrefix(name: ?[]const u8, buf: []u8) []const u8 {
        if (name) |n| {
            return std.fmt.bufPrint(buf, "'{s}' ", .{n}) catch "";
        }
        return "";
    }

    fn reportDoubleFree(ctx: *Context, allocation: Allocated, previous_free: Free) anyerror {
        try ctx.meta.print(ctx.writer, "double free in ", .{});
        // Use name_at_free for "previously freed" line
        var buf1: [256]u8 = undefined;
        const free_prefix = formatNamePrefix(previous_free.name_at_free, &buf1);
        if (free_prefix.len > 0) {
            try previous_free.meta.print(ctx.writer, "{s}previously freed in ", .{free_prefix});
        } else {
            try previous_free.meta.print(ctx.writer, "previously freed in ", .{});
        }
        // Use name_at_alloc for "originally allocated" line
        var buf2: [256]u8 = undefined;
        const alloc_prefix = formatNamePrefix(allocation.name_at_alloc, &buf2);
        if (alloc_prefix.len > 0) {
            try allocation.meta.print(ctx.writer, "{s}originally allocated in ", .{alloc_prefix});
        } else {
            try allocation.meta.print(ctx.writer, "originally allocated in ", .{});
        }
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: *Context, allocation: Allocated, free_site: Free) anyerror {
        try ctx.meta.print(ctx.writer, "use after free in ", .{});
        // Use name_at_free for "freed" line
        var buf1: [256]u8 = undefined;
        const free_prefix = formatNamePrefix(free_site.name_at_free, &buf1);
        if (free_prefix.len > 0) {
            try free_site.meta.print(ctx.writer, "{s}freed in ", .{free_prefix});
        } else {
            try free_site.meta.print(ctx.writer, "freed in ", .{});
        }
        // Use name_at_alloc for "allocated" line
        var buf2: [256]u8 = undefined;
        const alloc_prefix = formatNamePrefix(allocation.name_at_alloc, &buf2);
        if (alloc_prefix.len > 0) {
            try allocation.meta.print(ctx.writer, "{s}allocated in ", .{alloc_prefix});
        } else {
            try allocation.meta.print(ctx.writer, "allocated in ", .{});
        }
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: *Context, allocation: Allocated) anyerror {
        try ctx.meta.print(ctx.writer, "memory leak in ", .{});
        var buf: [256]u8 = undefined;
        const name_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
        if (name_prefix.len > 0) {
            try allocation.meta.print(ctx.writer, "{s}allocated in ", .{name_prefix});
        } else {
            try allocation.meta.print(ctx.writer, "allocated in ", .{});
        }
        return error.MemoryLeak;
    }

    fn reportMismatchedAllocator(ctx: *Context, allocation: Allocated, destroy_type_id: u32) anyerror {
        try ctx.meta.print(ctx.writer, "allocator mismatch in ", .{});
        const alloc_type_name = ctx.getName(allocation.type_id);
        try allocation.meta.print(ctx.writer, "allocated with {s} in ", .{alloc_type_name});
        var buf: [256]u8 = undefined;
        const destroy_type_name = ctx.getName(destroy_type_id);
        const msg = std.fmt.bufPrint(&buf, "freed with {s}\n", .{destroy_type_name}) catch return error.FormatError;
        try ctx.writer.writeAll(msg);
        return error.MismatchedAllocator;
    }

    fn reportFreeStackMemory(ctx: *Context, sp: Stack) anyerror {
        try ctx.meta.print(ctx.writer, "free of stack memory in ", .{});
        switch (sp.name) {
            .variable => |name_id| {
                const name = ctx.getName(name_id);
                try sp.meta.print(ctx.writer, "pointer is to local variable '{s}' in ", .{name});
            },
            .parameter => |name_id| {
                const name = ctx.getName(name_id);
                try sp.meta.print(ctx.writer, "pointer is to parameter '{s}' in ", .{name});
            },
            .other => {
                try sp.meta.print(ctx.writer, "pointer is to stack memory in ", .{});
            },
        }
        return error.FreeStackMemory;
    }

    fn reportFreeFieldPointer(ctx: *Context, allocation: Allocated) anyerror {
        try ctx.meta.print(ctx.writer, "free of field pointer in ", .{});
        try allocation.meta.print(ctx.writer, "pointer is to field of allocation from ", .{});
        return error.FreeFieldPointer;
    }

    fn reportFreeFieldPointerStack(ctx: *Context, sp: Stack) anyerror {
        try ctx.meta.print(ctx.writer, "free of field pointer in ", .{});
        try sp.meta.print(ctx.writer, "pointer is to field of stack allocation from ", .{});
        return error.FreeFieldPointer;
    }

    // =========================================================================
    // Branch merging
    // =========================================================================

    /// Merge memory_safety state from N branches for a single node.
    /// Called by tag.splatMerge which handles the tree traversal.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        refinements: *Refinements,
        orig_gid: Gid,
        branches: []const ?State,
        branch_gids: []const ?Gid,
    ) !void {
        _ = ctx;
        _ = merge_tag;
        const orig_ref = refinements.at(orig_gid);

        // Only pointer and scalar refinements have analytes
        const orig_analyte = switch (orig_ref.*) {
            .pointer => |*p| &p.analyte,
            .scalar => |*s| &s.analyte,
            else => return, // No memory_safety on container types
        };

        // Handle allocation freed state merging
        // If original has an unfreed allocation, check if all branches freed it
        if (orig_analyte.memory_safety) |*orig_ms| {
            if (orig_ms.* == .allocated) {
                const orig_alloc = &orig_ms.allocated;
                if (orig_alloc.freed == null) {
                    // Original not freed - check if all branches freed it
                    var all_freed = true;
                    var first_freed: ?Free = null;

                    for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                        // Null branch = unreachable path (e.g., unreach in cold branch)
                        const branch = branch_opt orelse continue;
                        // Entity may not exist in all branches during recursive merge traversal
                        const branch_gid = branch_gid_opt orelse continue;
                        const branch_ref = branch.refinements.at(branch_gid);
                        // Original was pointer/scalar with allocation, branch copy must be same type
                        const branch_analyte = switch (branch_ref.*) {
                            .pointer => |*p| &p.analyte,
                            .scalar => |*s| &s.analyte,
                            else => unreachable,
                        };
                        // Original has memory_safety with allocation, branch copy must too
                        const branch_ms = branch_analyte.memory_safety.?;
                        if (branch_ms.allocated.freed) |freed| {
                            if (first_freed == null) {
                                first_freed = freed;
                            }
                        } else {
                            all_freed = false;
                        }
                    }

                    // If all branches freed, propagate freed state
                    if (all_freed and first_freed != null) {
                        orig_alloc.freed = first_freed;
                    }
                    // Note: if only some branches freed, that's a leak in the
                    // non-freeing branch - onFinish will detect this
                }
            }
        } else {
            // Original has no memory_safety - copy from first branch that has it
            for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                // Null branch = unreachable path
                const branch = branch_opt orelse continue;
                // Entity may not exist in all branches during recursive merge traversal
                const branch_gid = branch_gid_opt orelse continue;
                const branch_ref = branch.refinements.at(branch_gid);
                // Original was pointer/scalar, branch copy must be same type
                const branch_analyte = switch (branch_ref.*) {
                    .pointer => |*p| &p.analyte,
                    .scalar => |*s| &s.analyte,
                    else => unreachable,
                };
                if (branch_analyte.memory_safety) |ms| {
                    orig_analyte.memory_safety = ms;
                    break;
                }
            }
        }
    }

    /// Handle orphaned entities detected during branch merge.
    /// An orphaned entity was created in a branch but is no longer reachable after merge.
    /// If the orphaned entity is a pointer to an unfreed allocation, report a leak.
    pub fn orphaned(ctx: *Context, refinements: *Refinements, branch_refinements: *Refinements, gid: Gid) !void {
        _ = refinements; // Main refinements not needed for immediate reporting
        const ref = branch_refinements.at(gid);

        // Only check pointers - allocation state is on the pointee
        if (ref.* != .pointer) return;

        // Get the pointee entity via ptr.to
        const pointee_idx = ref.pointer.to;
        const pointee = branch_refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Check pointee's memory_safety for allocation state
        const ms = pointee_analyte.memory_safety orelse return;
        if (ms != .allocated) return;

        const allocation = ms.allocated;
        // If allocation is not freed and not returned, it's a leak
        if (allocation.freed == null and !allocation.returned) {
            return reportMemoryLeak(ctx, allocation);
        }
    }
};

// =========================================================================
// Validation
// =========================================================================

const debug = @import("builtin").mode == .Debug;

/// Validate that a refinement conforms to memory_safety tracking rules.
/// TODO: Re-enable strict checking once all handlers set memory_safety.
/// With the new architecture:
/// - Non-trivial types (scalar, struct, union, optional, errorunion) SHOULD have memory_safety set
/// - Trivial types (void, unimplemented, noreturn, retval_future, region): no memory_safety
pub fn testValid(refinement: Refinements.Refinement) void {
    // Temporarily disabled - not all handlers set memory_safety yet
    _ = refinement;
}

// =========================================================================
// Tests
// =========================================================================

/// Test getName function that maps name IDs to strings for tests
fn testGetName(id: u32) []const u8 {
    return switch (id) {
        1 => "my_var",
        2 => "my_val",
        3 => "param",
        4 => "foo",
        5 => "src_var",
        6 => "local",
        7 => "container",
        // Allocator type IDs
        10 => "PageAllocator",
        11 => "ArenaAllocator",
        else => "unknown",
    };
}

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32, base_line: u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
    ctx.base_line = base_line;
    ctx.getName = &testGetName;
    return ctx;
}

fn testState(ctx: *Context, results: []Inst, refinements: *Refinements) State {
    return .{
        .ctx = ctx,
        .results = results,
        .refinements = refinements,
        .return_gid = 0,
    };
}

test "alloc sets stack metadata on pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer) then MemorySafety.alloc
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // Check pointer has .stack memory_safety
    const ptr = refinements.at(results[1].refinement.?).pointer;
    const ptr_ms = ptr.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(ptr_ms));
    try std.testing.expectEqualStrings("test_func", ptr_ms.stack.meta.function);

    // Check pointee also has .stack
    const pointee = refinements.at(ptr.to);
    const pointee_ms = pointee.scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.stack, std.meta.activeTag(pointee_ms));
    try std.testing.expectEqualStrings("test_func", pointee_ms.stack.meta.function);
    try std.testing.expectEqualStrings("test.zig", pointee_ms.stack.meta.file);
    try std.testing.expectEqual(@as(u32, 10), pointee_ms.stack.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), pointee_ms.stack.meta.column);
    try std.testing.expectEqual(@as(?Gid, null), pointee_ms.stack.root_gid); // root allocation
}

test "dbg_var_ptr sets variable name when name is other" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc to set up stack with .other name (on pointee)
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    const ptr1 = refinements.at(results[1].refinement.?).pointer;
    const pointee1 = refinements.at(ptr1.to);
    const ms1 = pointee1.scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack.name));

    // dbg_var_ptr should set the variable name on the pointee (name_id=4 -> "foo")
    try Inst.apply(state, 2, .{ .dbg_var_ptr = .{ .ptr = 1, .name_id = 4 } });

    const pointee2 = refinements.at(ptr1.to);
    const ms2 = pointee2.scalar.analyte.memory_safety.?;
    try std.testing.expectEqual(.variable, std.meta.activeTag(ms2.stack.name));
    try std.testing.expectEqual(@as(u32, 4), ms2.stack.name.variable);
}

test "bitcast propagates stack metadata via shared pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Set up source pointer with stack metadata on POINTEE (new architecture)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{
        .memory_safety = .{ .stack = .{
            .meta = .{
                .function = "source_func",
                .file = "source.zig",
                .line = 42,
                .column = 7,
            },
            .root_gid = null,
            .name = .{ .variable = 5 }, // 5 -> "src_var"
        } },
    }, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{}, // Pointer has no memory_safety, it's on pointee
        .type_id = 0,
        .to = pointee_idx,
    } });

    // Bitcast shares the refinement - both point to the same pointee
    try Inst.apply(state, 1, .{ .bitcast = .{ .src = .{ .inst = 0 }, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // Check that both pointers share the same pointee with memory_safety
    const ptr1 = refinements.at(results[0].refinement.?).pointer;
    const ptr2 = refinements.at(results[1].refinement.?).pointer;
    try std.testing.expectEqual(ptr1.to, ptr2.to); // Same pointee

    // Check the shared pointee has the memory_safety
    const pointee = refinements.at(ptr2.to);
    const ms = pointee.scalar.analyte.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack.meta.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack.meta.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack.meta.line);
    try std.testing.expectEqual(@as(u32, 5), ms.stack.name.variable); // 5 -> "src_var"
}

test "ret_safe detects escape when returning stack pointer from same function" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Pointer with stack_ptr from test_func (current function)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack = .{
            .meta = .{
                .function = "test_func",
                .file = "test.zig",
                .line = 5,
            },
            .root_gid = null,
            .name = .{ .variable = 6 }, // 6 -> "local"
        } } },
        .type_id = 0,
        .to = pointee_idx,
    } });

    const state = testState(&ctx, &results, &refinements);
    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(state, 1, .{ .src = .{ .inst = 0 } }),
    );
}

test "ret_safe allows returning arg (empty function name)" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Pointer with empty function name (from caller via arg)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack = .{
            .meta = .{
                .function = "",
                .file = "test.zig",
                .line = 5,
            },
            .root_gid = null,
            .name = .{ .parameter = 3 }, // 3 -> "param"
        } } },
        .type_id = 0,
        .to = pointee_idx,
    } });

    // Should NOT error - returning pointer from caller is fine
    const state = testState(&ctx, &results, &refinements);
    try MemorySafety.ret_safe(state, 1, .{ .src = .{ .inst = 0 } });
}

test "alloc_create sets allocation metadata on pointer analyte" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    try Inst.apply(state, 1, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // alloc_create creates errorunion -> ptr -> pointee, all with .allocated
    const eu_idx = results[1].refinement.?;
    const eu_ref = refinements.at(eu_idx);
    const ptr_idx = eu_ref.errorunion.to;
    const ptr_ref = refinements.at(ptr_idx);
    const pointee_idx = ptr_ref.pointer.to;

    // Check errorunion has .allocated
    const eu_ms = eu_ref.errorunion.analyte.memory_safety.?;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(eu_ms));

    // Check pointer has .allocated
    const ptr_ms = ptr_ref.pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(ptr_ms));
    try std.testing.expectEqual(@as(u32, 10), ptr_ms.allocated.type_id);

    // Check pointee has .allocated
    const pointee_ref = refinements.at(pointee_idx);
    const pointee_analyte = MemorySafety.getAnalytePtr(pointee_ref);
    const pointee_ms = pointee_analyte.memory_safety.?;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(pointee_ms));
    try std.testing.expectEqual(@as(u32, 10), pointee_ms.allocated.type_id);
    try std.testing.expectEqualStrings("test.zig", pointee_ms.allocated.meta.file);
    try std.testing.expectEqual(@as(u32, 10), pointee_ms.allocated.meta.line);
    try std.testing.expectEqual(@as(?Free, null), pointee_ms.allocated.freed);
}

test "alloc_destroy marks allocation as freed" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation (errorunion -> ptr -> pointee)
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    // Unwrap error union to get the pointer (simulating real AIR flow)
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });

    // Update context for free location
    ctx.meta.line = 20;

    // Destroy allocation (ptr points to unwrapped pointer at inst 1)
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 10 } });

    // With new architecture, allocation state is on the POINTEE (accessed via ptr.to)
    const ptr_idx = results[1].refinement.?;
    const ptr_ref = refinements.at(ptr_idx);
    const pointee_idx = ptr_ref.pointer.to;
    const pointee = refinements.at(pointee_idx);
    const pointee_analyte = MemorySafety.getAnalytePtr(pointee);
    const pointee_ms = pointee_analyte.memory_safety.?;
    try std.testing.expectEqual(.allocated, std.meta.activeTag(pointee_ms));
    try std.testing.expect(pointee_ms.allocated.freed != null);
    try std.testing.expectEqual(@as(u32, 20), pointee_ms.allocated.freed.?.meta.line);
}

test "alloc_destroy detects double free" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation (errorunion -> ptr -> pointee)
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    // Unwrap error union to get the pointer (simulating real AIR flow)
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });

    // First free
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 10 } });

    // Second free should error
    try std.testing.expectError(
        error.DoubleFree,
        Inst.apply(state, 3, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 10 } }),
    );
}

test "alloc_destroy detects mismatched allocator" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create with PageAllocator and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });

    // Destroy with different allocator
    try std.testing.expectError(
        error.MismatchedAllocator,
        Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 11 } }),
    );
}

test "alloc_destroy detects freeing stack memory" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create stack allocation (alloc, not alloc_create)
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // Trying to free stack memory should error
    try std.testing.expectError(
        error.FreeStackMemory,
        Inst.apply(state, 1, .{ .alloc_destroy = .{ .ptr = 0, .type_id = 10 } }),
    );
}

test "load detects use after free" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 6;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, store (to make it defined), and free allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });
    try Inst.apply(state, 3, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 10 } });

    // Load after free should error
    try std.testing.expectError(
        error.UseAfterFree,
        Inst.apply(state, 4, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } }),
    );
}

test "load from live allocation does not error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, and store to allocation (not freed)
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Load from live allocation should succeed
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
}

test "onFinish detects memory leak" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation and unwrap but don't free
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });

    // onFinish should detect the leak (via the unwrapped pointer at inst 1)
    try std.testing.expectError(
        error.MemoryLeak,
        MemorySafety.onFinish(&results, &ctx, &refinements),
    );
}

test "onFinish allows freed allocation" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, and free allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    try Inst.apply(state, 2, .{ .alloc_destroy = .{ .ptr = 1, .type_id = 10 } });

    // onFinish should not error
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "onFinish allows passed allocation" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    // Global refinements table - return slot pre-allocated
    var refinements = Refinements.init(allocator);
    defer refinements.deinit();
    const return_gid = try refinements.appendEntity(.{ .retval_future = {} });

    var results = [_]Inst{.{}} ** 5;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_gid = return_gid,
    };

    // Create allocation and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    // Store to make the pointee defined
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Return the pointer (marks as passed)
    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .inst = 1 } } });

    // onFinish should not error - allocation was passed to caller
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "onFinish ignores stack allocations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    try ctx.stacktrace.append(allocator, "test_func");
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 2;
    const state = testState(&ctx, &results, &refinements);

    // Create stack allocation (not heap)
    try Inst.apply(state, 0, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    // onFinish should not error - stack memory is fine
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "load from struct field shares pointer entity - freeing loaded pointer marks original as freed" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 11;
    const state = testState(&ctx, &results, &refinements);

    // === SETUP: struct with pointer field pointing to allocation ===
    // inst 0: alloc_create - create heap allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    // inst 1: unwrap_errunion_payload - get the pointer
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    // inst 2: store_safe - make pointee defined
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // inst 3: alloc - create struct on stack with a pointer field
    const struct_ty: tag.Type = .{ .id = null, .ty = .{ .@"struct" = &.{.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } }} } };
    try Inst.apply(state, 3, .{ .alloc = .{ .ty = struct_ty } });
    // inst 4: store_safe - initialize struct with undefined
    try Inst.apply(state, 4, .{ .store_safe = .{ .ptr = 3, .src = .{ .interned = .{ .id = null, .ty = .{ .undefined = &struct_ty } } } } });

    // inst 5: struct_field_ptr - get pointer to field 0
    try Inst.apply(state, 5, .{ .struct_field_ptr = .{
        .base = 3,
        .field_index = 0,
        .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } },
    } });
    // inst 6: store_safe - store the allocation pointer into struct field
    try Inst.apply(state, 6, .{ .store_safe = .{ .ptr = 5, .src = .{ .inst = 1 } } });

    // === LOAD POINTER FROM STRUCT FIELD ===
    // inst 7: load - load struct from stack alloc
    try Inst.apply(state, 7, .{ .load = .{ .ptr = 3, .ty = struct_ty } });
    // inst 8: struct_field_val - get pointer field value
    try Inst.apply(state, 8, .{ .struct_field_val = .{ .operand = 7, .field_index = 0, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } });

    // === FREE THE LOADED POINTER ===
    // inst 9: alloc_destroy - free via the loaded pointer
    try Inst.apply(state, 9, .{ .alloc_destroy = .{ .ptr = 8, .type_id = 10 } });

    // === VERIFICATION ===
    // The original struct field pointer (stored via inst 6) should also be marked as freed.
    // This requires that load shares the pointer entity instead of copying it.
    //
    // Load the struct again and get the field - check its memory_safety state
    try Inst.apply(state, 10, .{ .load = .{ .ptr = 3, .ty = struct_ty } });

    // Get the struct field pointer from the original struct (via struct_field_ptr at inst 5)
    const field_ptr_ref = results[5].refinement.?;
    const field_ptr = refinements.at(field_ptr_ref);

    // The field pointer points to the pointer entity
    const ptr_entity_idx = field_ptr.pointer.to;
    const ptr_entity = refinements.at(ptr_entity_idx);
    try std.testing.expect(ptr_entity.* == .pointer);

    // With new architecture, check the POINTEE's allocation state (via ptr.to)
    const pointee_idx = ptr_entity.pointer.to;
    const pointee = refinements.at(pointee_idx);
    const pointee_analyte = MemorySafety.getAnalytePtr(pointee);
    const ms = pointee_analyte.memory_safety orelse {
        return error.ExpectedMemorySafetyState;
    };
    try std.testing.expect(ms == .allocated);
    // This is the key assertion: the allocation should be marked as freed
    try std.testing.expect(ms.allocated.freed != null);
}

test "global refinements: freeing struct pointer field is visible immediately" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    // Global refinements table shared by all code
    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 8;
    const state = testState(&ctx, &results, &refinements);

    // inst 0 = alloc_create, inst 1 = unwrap to get pointer
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .inst = 0 } } });
    // inst 1 now has the allocation pointer

    // inst 2 = alloc struct with pointer field
    const struct_ty: tag.Type = .{ .id = null, .ty = .{ .@"struct" = &.{.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } }} } };
    try Inst.apply(state, 2, .{ .alloc = .{ .ty = struct_ty } });
    // inst 3 = store undefined struct
    try Inst.apply(state, 3, .{ .store_safe = .{ .ptr = 2, .src = .{ .interned = .{ .id = null, .ty = .{ .undefined = &struct_ty } } } } });
    // inst 4 = struct_field_ptr to field 0
    try Inst.apply(state, 4, .{ .struct_field_ptr = .{ .base = 2, .field_index = 0, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } } } });
    // inst 5 = store allocation pointer into struct field
    try Inst.apply(state, 5, .{ .store_safe = .{ .ptr = 4, .src = .{ .inst = 1 } } });

    // Get the allocation's POINTEE entity for later verification
    const alloc_ptr_ref = results[1].refinement.?;
    const alloc_ptr = refinements.at(alloc_ptr_ref);
    const alloc_pointee_idx = alloc_ptr.pointer.to;

    // Verify allocation is not freed yet
    const pointee_before = refinements.at(alloc_pointee_idx);
    const pointee_analyte_before = MemorySafety.getAnalytePtr(pointee_before);
    const pointee_ms_before = pointee_analyte_before.memory_safety.?;
    try std.testing.expect(pointee_ms_before == .allocated);
    try std.testing.expect(pointee_ms_before.allocated.freed == null);

    // inst 6 = load from struct field to get the pointer
    try Inst.apply(state, 6, .{ .load = .{ .ptr = 4, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } });
    // inst 7 = alloc_destroy to free the pointer
    try Inst.apply(state, 7, .{ .alloc_destroy = .{ .ptr = 6, .type_id = 10 } });

    // With global refinements, the allocation should be marked as freed immediately
    // No backpropagation needed - modifications are direct
    const pointee_after = refinements.at(alloc_pointee_idx);
    const pointee_analyte_after = MemorySafety.getAnalytePtr(pointee_after);
    const pointee_ms_after = pointee_analyte_after.memory_safety.?;
    try std.testing.expect(pointee_ms_after == .allocated);
    // This is the key assertion: the allocation should be marked as freed
    try std.testing.expect(pointee_ms_after.allocated.freed != null);
}
