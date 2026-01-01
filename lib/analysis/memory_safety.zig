const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Analyte = @import("../Analyte.zig");
const EIdx = Inst.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// =========================================================================
// State types
// =========================================================================

pub const StackPtr = struct {
    meta: Meta,
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

pub const Allocation = struct {
    allocated: Meta,
    freed: ?Free = null, // null = still allocated, has value = freed
    type_id: u32, // Allocator type ID, resolved via ctx.getName() for error messages
    name_at_alloc: ?[]const u8 = null, // Full path name when allocated (e.g., "container.ptr")
    returned: bool = false, // true if this allocation was returned to caller (not a local leak)
};

pub const MemorySafety = union(enum) {
    stack_ptr: StackPtr,
    allocation: Allocation,

    pub fn alloc(state: State, index: usize, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, set memory_safety on the pointer's analyte
        const ptr_idx = state.results[index].refinement.?;
        state.refinements.at(ptr_idx).pointer.analyte.memory_safety = .{ .stack_ptr = .{ .meta = state.ctx.meta } };
    }

    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        // ptr is null for stores to interned/global locations - no memory safety tracking needed
        const ptr = params.ptr orelse return;
        const src = switch (params.src) {
            .eidx => |idx| idx,
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
                if (ms.* == .allocation) {
                    // Set name_at_alloc if not already set
                    if (ms.allocation.name_at_alloc == null) {
                        ms.allocation.name_at_alloc = ctx.buildPathName(results, refinements, ptr);
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
            if (ms.* == .stack_ptr) {
                ms.stack_ptr.name = .{ .parameter = param_name_id };
                ms.stack_ptr.meta = .{
                    .function = ctx.meta.function,
                    .file = ctx.meta.file,
                    .line = ctx.base_line + 1,
                    .column = null,
                };
            }
        }
    }

    /// Retroactively set variable name on stack_ptr for escape detection messages.
    /// The name_id is already set on the instruction by DbgVarPtr.apply().
    pub fn dbg_var_ptr(state: State, index: usize, params: tag.DbgVarPtrParams) !void {
        _ = index;
        const inst = params.ptr orelse return;
        const ptr_idx = state.results[inst].refinement orelse return;
        if (state.refinements.at(ptr_idx).* == .pointer) {
            const outer_analyte = &state.refinements.at(ptr_idx).pointer.analyte;
            if (outer_analyte.memory_safety) |*ms| {
                if (ms.* == .stack_ptr) {
                    if (ms.stack_ptr.name == .other) {
                        ms.stack_ptr.name = .{ .variable = params.name_id };
                    }
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

    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const src = switch (params.src) {
            .eidx => |idx| idx,
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
    fn markAllocationsAsReturned(refinements: *Refinements, idx: EIdx, ctx: *Context) !void {
        const refinement = refinements.at(idx);
        switch (refinement.*) {
            .pointer => |*p| {
                // Check pointer's memory_safety for stack pointer escape
                if (p.analyte.memory_safety) |ms| {
                    if (ms == .stack_ptr) {
                        const sp = ms.stack_ptr;
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
                    if (ms.* == .allocation) {
                        // Mark the allocation as returned - not a leak in this function
                        ms.allocation.returned = true;
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
    fn checkStackEscapeRecursive(refinements: *Refinements, idx: EIdx, ctx: *Context, func_name: []const u8) !void {
        switch (refinements.at(idx).*) {
            .pointer => |p| {
                const ms = p.analyte.memory_safety orelse return;
                switch (ms) {
                    .stack_ptr => |sp| {
                        if (std.mem.eql(u8, sp.meta.function, func_name)) {
                            return reportStackEscape(ms, ctx);
                        }
                    },
                    .allocation => {},
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
    /// Backward propagation is handled centrally by Inst.backPropagate().
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
        // Check for stack pointer escapes via pointer arguments
        // If a pointer arg's pointee contains a stack pointer from this function,
        // backPropagate would escape it to the caller
        if (ctx.stacktrace.items.len > 0) {
            const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
            for (results) |inst| {
                // Only check args that have caller entity (will be back-propagated)
                if (inst.caller_eidx == null) continue;
                const idx = inst.refinement orelse continue;
                const refinement = refinements.at(idx);

                // Only check pointer args - scalar args are copied by value
                if (refinement.* != .pointer) continue;

                // Check the pointee for stack pointers
                try checkStackEscapeRecursive(refinements, refinement.pointer.to, ctx, func_name);
            }
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
            if (ms != .allocation) continue;

            const allocation = ms.allocation;
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
        const ptr_ref = state.refinements.at(eu_idx).errorunion.to;
        const ptr = &state.refinements.at(ptr_ref).pointer;
        const pointee_ref = ptr.to;
        const pointee = state.refinements.at(pointee_ref);

        // Set allocation state on the POINTEE - this is the shared state all pointers will reference
        const pointee_analyte = getAnalytePtr(pointee);
        pointee_analyte.memory_safety = .{ .allocation = .{
            .allocated = state.ctx.meta,
            .type_id = params.type_id,
        } };
        // Pointer's memory_safety remains null - we look up via ptr.to
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

        // Check if pointer itself is a stack pointer (freeing stack memory is an error)
        if (ptr_refinement.* == .pointer) {
            if (ptr_refinement.pointer.analyte.memory_safety) |ms| {
                if (ms == .stack_ptr) {
                    return reportFreeStackMemory(ctx, ms.stack_ptr);
                }
            }
        }

        // Get the pointee entity via ptr.to
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee = refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Get allocation state from pointee
        const ms = &(pointee_analyte.memory_safety orelse
            @panic("alloc_destroy: pointee has no memory_safety"));

        if (ms.* != .allocation) {
            @panic("alloc_destroy: pointee memory_safety is not allocation");
        }

        const a = ms.allocation;
        if (a.freed) |previous_free| {
            return reportDoubleFree(ctx, a, previous_free);
        }
        if (a.type_id != params.type_id) {
            return reportMismatchedAllocator(ctx, a, params.type_id);
        }

        // Mark as freed
        ms.allocation.freed = .{
            .meta = ctx.meta,
            .name_at_free = ctx.buildPathName(results, refinements, ptr),
        };
    }

    /// Handle load - detect use-after-free.
    /// Checks the POINTEE's allocation state (via ptr.to) for freed status.
    pub fn load(state: State, index: usize, params: tag.Load) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
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
        if (ms != .allocation) return;

        if (ms.allocation.freed) |free_site| {
            return reportUseAfterFree(ctx, ms.allocation, free_site);
        }
    }

    // =========================================================================
    // Error reporting
    // =========================================================================

    fn reportStackEscape(ms: MemorySafety, ctx: *Context) anyerror {
        const sp = ms.stack_ptr;
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

    fn reportDoubleFree(ctx: *Context, allocation: Allocation, previous_free: Free) anyerror {
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
            try allocation.allocated.print(ctx.writer, "{s}originally allocated in ", .{alloc_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "originally allocated in ", .{});
        }
        return error.DoubleFree;
    }

    fn reportUseAfterFree(ctx: *Context, allocation: Allocation, free_site: Free) anyerror {
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
            try allocation.allocated.print(ctx.writer, "{s}allocated in ", .{alloc_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "allocated in ", .{});
        }
        return error.UseAfterFree;
    }

    fn reportMemoryLeak(ctx: *Context, allocation: Allocation) anyerror {
        try ctx.meta.print(ctx.writer, "memory leak in ", .{});
        var buf: [256]u8 = undefined;
        const name_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
        if (name_prefix.len > 0) {
            try allocation.allocated.print(ctx.writer, "{s}allocated in ", .{name_prefix});
        } else {
            try allocation.allocated.print(ctx.writer, "allocated in ", .{});
        }
        return error.MemoryLeak;
    }

    fn reportMismatchedAllocator(ctx: *Context, allocation: Allocation, destroy_type_id: u32) anyerror {
        try ctx.meta.print(ctx.writer, "allocator mismatch in ", .{});
        const alloc_type_name = ctx.getName(allocation.type_id);
        try allocation.allocated.print(ctx.writer, "allocated with {s} in ", .{alloc_type_name});
        var buf: [256]u8 = undefined;
        const destroy_type_name = ctx.getName(destroy_type_id);
        const msg = std.fmt.bufPrint(&buf, "freed with {s}\n", .{destroy_type_name}) catch return error.FormatError;
        try ctx.writer.writeAll(msg);
        return error.MismatchedAllocator;
    }

    fn reportFreeStackMemory(ctx: *Context, sp: StackPtr) anyerror {
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

    // =========================================================================
    // Branch merging
    // =========================================================================

    /// Merge memory_safety state from N branches for a single node.
    /// Called by tag.splatMerge which handles the tree traversal.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        refinements: *Refinements,
        orig_eidx: EIdx,
        branches: []const ?State,
        branch_eidxs: []const ?EIdx,
    ) !void {
        _ = ctx;
        _ = merge_tag;
        const orig_ref = refinements.at(orig_eidx);

        switch (orig_ref.*) {
            .pointer => |*op| {
                // Copy memory_safety from first branch that has it if original doesn't
                if (op.analyte.memory_safety == null) {
                    for (branches, branch_eidxs) |branch_opt, branch_eidx_opt| {
                        const branch = branch_opt orelse continue;
                        const branch_eidx = branch_eidx_opt orelse continue;
                        const branch_ref = branch.refinements.at(branch_eidx);
                        if (branch_ref.* != .pointer) continue;
                        if (branch_ref.pointer.analyte.memory_safety) |ms| {
                            op.analyte.memory_safety = ms;
                            break;
                        }
                    }
                }
            },
            .scalar => |*s| {
                // Copy memory_safety from first branch that has it if original doesn't
                if (s.analyte.memory_safety == null) {
                    for (branches, branch_eidxs) |branch_opt, branch_eidx_opt| {
                        const branch = branch_opt orelse continue;
                        const branch_eidx = branch_eidx_opt orelse continue;
                        const branch_ref = branch.refinements.at(branch_eidx);
                        if (branch_ref.* != .scalar) continue;
                        if (branch_ref.scalar.analyte.memory_safety) |ms| {
                            s.analyte.memory_safety = ms;
                            break;
                        }
                    }
                }
            },
            // No memory_safety on container types - recursion handled by tag.zig
            else => {},
        }
    }

    /// Handle orphaned entities detected during branch merge.
    /// An orphaned entity was created in a branch but is no longer reachable after merge.
    /// If the orphaned entity is a pointer to an unfreed allocation, report a leak.
    pub fn orphaned(ctx: *Context, refinements: *Refinements, branch_refinements: *Refinements, eidx: EIdx) !void {
        _ = refinements; // Main refinements not needed for immediate reporting
        const ref = branch_refinements.at(eidx);

        // Only check pointers - allocation state is on the pointee
        if (ref.* != .pointer) return;

        // Get the pointee entity via ptr.to
        const pointee_idx = ref.pointer.to;
        const pointee = branch_refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Check pointee's memory_safety for allocation state
        const ms = pointee_analyte.memory_safety orelse return;
        if (ms != .allocation) return;

        const allocation = ms.allocation;
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

/// Validate that a refinement conforms to memory_safety tracking rules:
/// - Pointers CAN have memory_safety (stack_ptr for stack pointers)
/// - Scalars CAN have memory_safety (allocation state for heap pointees)
/// - Containers (optional, errorunion, struct) must NOT have memory_safety at the container level
pub fn testValid(refinement: Refinements.Refinement) void {
    if (!debug) return;
    switch (refinement) {
        .pointer => {
            // Pointers CAN have memory_safety (stack_ptr for stack pointers)
            // Heap allocation tracking is on the pointee, not the pointer
        },
        .scalar => {
            // Scalars CAN have memory_safety (allocation state for heap pointees)
        },
        inline .optional, .errorunion => |data, t| {
            if (data.analyte.memory_safety != null) {
                std.debug.panic("memory_safety should not exist on container types, got {s}", .{@tagName(t)});
            }
        },
        .@"struct" => |s| {
            if (s.analyte.memory_safety != null) {
                std.debug.panic("memory_safety should not exist on container types, got struct", .{});
            }
        },
        else => {},
    }
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
        .return_eidx = 0,
        .caller_refinements = null,
    };
}

test "alloc sets stack_ptr metadata on pointer analyte" {
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

    const ms = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqualStrings("test_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("test.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 10), ms.stack_ptr.meta.line);
    try std.testing.expectEqual(@as(?u32, 5), ms.stack_ptr.meta.column);
    try std.testing.expectEqual(.other, std.meta.activeTag(ms.stack_ptr.name));
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

    // First alloc to set up stack_ptr with .other name
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    const ms1 = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.other, std.meta.activeTag(ms1.stack_ptr.name));

    // dbg_var_ptr should set the variable name (name_id=4 -> "foo")
    try Inst.apply(state, 2, .{ .dbg_var_ptr = .{ .ptr = 1, .name_id = 4 } });

    const ms2 = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqual(.variable, std.meta.activeTag(ms2.stack_ptr.name));
    try std.testing.expectEqual(@as(u32, 4), ms2.stack_ptr.name.variable);
}

test "bitcast propagates stack_ptr metadata" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Set up source pointer with stack_ptr on analyte
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{}, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .pointer = .{
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "source_func",
                .file = "source.zig",
                .line = 42,
                .column = 7,
            },
            .name = .{ .variable = 5 }, // 5 -> "src_var"
        } } },
        .type_id = 0,
        .to = pointee_idx,
    } });

    // Bitcast shares the refinement
    try Inst.apply(state, 1, .{ .bitcast = .{ .src = .{ .eidx = 0 }, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });

    const ms = refinements.at(results[1].refinement.?).pointer.analyte.memory_safety.?;
    try std.testing.expectEqualStrings("source_func", ms.stack_ptr.meta.function);
    try std.testing.expectEqualStrings("source.zig", ms.stack_ptr.meta.file);
    try std.testing.expectEqual(@as(u32, 42), ms.stack_ptr.meta.line);
    try std.testing.expectEqual(@as(u32, 5), ms.stack_ptr.name.variable); // 5 -> "src_var"
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
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "test_func",
                .file = "test.zig",
                .line = 5,
            },
            .name = .{ .variable = 6 }, // 6 -> "local"
        } } },
        .type_id = 0,
        .to = pointee_idx,
    } });

    const state = testState(&ctx, &results, &refinements);
    try std.testing.expectError(
        error.StackPointerEscape,
        MemorySafety.ret_safe(state, 1, .{ .src = .{ .eidx = 0 } }),
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
        .analyte = .{ .memory_safety = .{ .stack_ptr = .{
            .meta = .{
                .function = "",
                .file = "test.zig",
                .line = 5,
            },
            .name = .{ .parameter = 3 }, // 3 -> "param"
        } } },
        .type_id = 0,
        .to = pointee_idx,
    } });

    // Should NOT error - returning pointer from caller is fine
    const state = testState(&ctx, &results, &refinements);
    try MemorySafety.ret_safe(state, 1, .{ .src = .{ .eidx = 0 } });
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

    // alloc_create creates errorunion -> ptr -> pointee
    // With new architecture: allocation state is on the POINTEE, pointer's memory_safety is null
    const eu_idx = results[1].refinement.?;
    const ptr_idx = refinements.at(eu_idx).errorunion.to;
    const ptr_ref = refinements.at(ptr_idx);
    const pointee_idx = ptr_ref.pointer.to;

    // Pointer's memory_safety is null for heap allocations
    try std.testing.expectEqual(@as(?MemorySafety, null), ptr_ref.pointer.analyte.memory_safety);

    // Check pointee has .allocation
    const pointee_ref = refinements.at(pointee_idx);
    const pointee_analyte = MemorySafety.getAnalytePtr(pointee_ref);
    const pointee_ms = pointee_analyte.memory_safety.?;
    try std.testing.expectEqual(.allocation, std.meta.activeTag(pointee_ms));
    try std.testing.expectEqual(@as(u32, 10), pointee_ms.allocation.type_id);
    try std.testing.expectEqualStrings("test.zig", pointee_ms.allocation.allocated.file);
    try std.testing.expectEqual(@as(u32, 10), pointee_ms.allocation.allocated.line);
    try std.testing.expectEqual(@as(?Free, null), pointee_ms.allocation.freed);
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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

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
    try std.testing.expectEqual(.allocation, std.meta.activeTag(pointee_ms));
    try std.testing.expect(pointee_ms.allocation.freed != null);
    try std.testing.expectEqual(@as(u32, 20), pointee_ms.allocation.freed.?.meta.line);
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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Load from live allocation should succeed
    try Inst.apply(state, 3, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
}

test "onFinish detects memory leak" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Create allocation and unwrap but don't free
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });

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
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 4;
    const state = testState(&ctx, &results, &refinements);

    // Create, unwrap, and free allocation
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
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

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();
    const return_eidx = try caller_refinements.appendEntity(.{ .retval_future = {} });

    var results = [_]Inst{.{}} ** 5;
    const state = State{
        .ctx = &ctx,
        .results = &results,
        .refinements = &refinements,
        .return_eidx = return_eidx,
        .caller_refinements = &caller_refinements,
    };

    // Create allocation and unwrap
    try Inst.apply(state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // Store to make the pointee defined
    try Inst.apply(state, 2, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = null, .ty = .{ .scalar = {} } } } } });

    // Return the pointer (marks as passed)
    try Inst.apply(state, 3, .{ .ret_safe = .{ .src = .{ .eidx = 1 } } });

    // onFinish should not error - allocation was passed to caller
    try MemorySafety.onFinish(&results, &ctx, &refinements);
}

test "onFinish ignores stack allocations" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
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
    try Inst.apply(state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
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
    try Inst.apply(state, 6, .{ .store_safe = .{ .ptr = 5, .src = .{ .eidx = 1 } } });

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
    try std.testing.expect(ms == .allocation);
    // This is the key assertion: the allocation should be marked as freed
    try std.testing.expect(ms.allocation.freed != null);
}

test "interprocedural: callee freeing struct pointer field propagates back to caller" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5, 0);
    defer ctx.deinit();

    // === CALLER SETUP ===
    var caller_refinements = Refinements.init(allocator);
    defer caller_refinements.deinit();

    var caller_results = [_]Inst{.{}} ** 6;
    const caller_state = testState(&ctx, &caller_results, &caller_refinements);

    // Caller: inst 0 = alloc_create, inst 1 = unwrap to get pointer
    try Inst.apply(caller_state, 0, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = null, .ty = .{ .scalar = {} } } } });
    try Inst.apply(caller_state, 1, .{ .unwrap_errunion_payload = .{ .src = .{ .eidx = 0 } } });
    // inst 1 now has the allocation pointer

    // Caller: inst 2 = alloc struct with pointer field
    const struct_ty: tag.Type = .{ .id = null, .ty = .{ .@"struct" = &.{.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } }} } };
    try Inst.apply(caller_state, 2, .{ .alloc = .{ .ty = struct_ty } });
    // inst 3 = store undefined struct
    try Inst.apply(caller_state, 3, .{ .store_safe = .{ .ptr = 2, .src = .{ .interned = .{ .id = null, .ty = .{ .undefined = &struct_ty } } } } });
    // inst 4 = struct_field_ptr to field 0
    try Inst.apply(caller_state, 4, .{ .struct_field_ptr = .{ .base = 2, .field_index = 0, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } } } });
    // inst 5 = store allocation pointer into struct field
    try Inst.apply(caller_state, 5, .{ .store_safe = .{ .ptr = 4, .src = .{ .eidx = 1 } } });

    // Get the allocation's POINTEE entity for later verification
    const alloc_ptr_ref = caller_results[1].refinement.?;
    const alloc_ptr = caller_refinements.at(alloc_ptr_ref);
    const alloc_pointee_idx = alloc_ptr.pointer.to;

    // Verify allocation is not freed yet (check POINTEE's memory_safety)
    const pointee_before = caller_refinements.at(alloc_pointee_idx);
    const pointee_analyte_before = MemorySafety.getAnalytePtr(pointee_before);
    const pointee_ms_before = pointee_analyte_before.memory_safety.?;
    try std.testing.expect(pointee_ms_before == .allocation);
    try std.testing.expect(pointee_ms_before.allocation.freed == null); // Not freed yet

    // === CALLEE SETUP ===
    var callee_refinements = Refinements.init(allocator);
    defer callee_refinements.deinit();

    var callee_results = [_]Inst{.{}} ** 5;

    // Copy caller's struct pointer using copyTo (simulating arg)
    const caller_ptr_idx = caller_results[2].refinement.?;
    const local_ptr_idx = try caller_refinements.at(caller_ptr_idx).*.copyTo(&caller_refinements, &callee_refinements);
    callee_results[0].refinement = local_ptr_idx;
    callee_results[0].caller_eidx = caller_ptr_idx;
    callee_results[0].name_id = 7; // 7 -> "container"

    const callee_state = State{
        .ctx = &ctx,
        .results = &callee_results,
        .refinements = &callee_refinements,
        .return_eidx = 0,
        .caller_refinements = &caller_refinements,
    };

    // Callee: inst 1 = struct_field_ptr to get pointer to field 0
    try Inst.apply(callee_state, 1, .{ .struct_field_ptr = .{ .base = 0, .field_index = 0, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } } } });
    // Callee: inst 2 = load to get the pointer value from field
    try Inst.apply(callee_state, 2, .{ .load = .{ .ptr = 1, .ty = .{ .id = null, .ty = .{ .pointer = &.{ .id = null, .ty = .{ .scalar = {} } } } } } });
    // Callee: inst 3 = alloc_destroy to free the pointer
    try Inst.apply(callee_state, 3, .{ .alloc_destroy = .{ .ptr = 2, .type_id = 10 } });

    // Callee's local pointer's POINTEE should be freed
    const local_loaded_ptr_ref = callee_results[2].refinement.?;
    const local_loaded_ptr = callee_refinements.at(local_loaded_ptr_ref);
    try std.testing.expect(local_loaded_ptr.* == .pointer);
    const local_pointee_idx = local_loaded_ptr.pointer.to;
    const local_pointee = callee_refinements.at(local_pointee_idx);
    const local_pointee_analyte = MemorySafety.getAnalytePtr(local_pointee);
    const local_pointee_ms = local_pointee_analyte.memory_safety.?;
    try std.testing.expect(local_pointee_ms == .allocation);
    try std.testing.expect(local_pointee_ms.allocation.freed != null);

    // === BACKPROPAGATE ===
    Inst.backPropagate(callee_state);

    // === VERIFY CALLER'S STATE IS UPDATED ===
    // The caller's allocation pointee should now be marked as freed
    const pointee_after = caller_refinements.at(alloc_pointee_idx);
    const pointee_analyte_after = MemorySafety.getAnalytePtr(pointee_after);
    const pointee_ms_after = pointee_analyte_after.memory_safety.?;
    try std.testing.expect(pointee_ms_after == .allocation);
    // This is the key assertion: the caller's allocation should be marked as freed
    try std.testing.expect(pointee_ms_after.allocation.freed != null);
}
