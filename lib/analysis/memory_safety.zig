const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Analyte = @import("../Analyte.zig");
const Gid = Refinements.Gid;
const core = @import("../core.zig");
const Meta = core.Meta;
const tag = @import("../tag.zig");
const gates = @import("gates.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// =========================================================================
// State types
// =========================================================================

pub const Free = struct {
    meta: Meta,
    name_at_free: ?[]const u8 = null, // Full path name (arena-allocated at store time)
};

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

pub const Allocated = struct {
    meta: Meta,
    root_gid: ?Gid, // null = root, else parent container gid
    freed: ?Free = null, // null = still allocated, has value = freed

    allocator_gid: Gid, // GID of the allocator used for this allocation (for mismatch detection)
    type_id: u32, // Type ID for error messages (from allocator refinement's type_id)
    name_at_alloc: ?[]const u8 = null, // Full path name when allocated (e.g., "container.ptr")
    returned: bool = false, // true if this allocation was returned to caller (not a local leak)
    is_slice: bool = false, // true if allocated with alloc (slice), false if create (single item)
};

pub const MemorySafety = union(enum) {
    stack: Stack,
    interned: Meta, // Comptime/interned values (globals, string literals, etc.)
    allocated: Allocated,
    unset: void,

    /// only allowed on error unions.
    /// marks errorunions from allocation instructions.
    /// On error path, these are "phantom" allocations that didn't actually happen.
    /// cond_br looks up the errorunion directly via is_non_err.src from inst_tag.
    error_stub: void,

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(&.{@intFromEnum(self)});
        switch (self) {
            .allocated => |a| {
                hasher.update(&.{@as(u8, if (a.freed != null) 1 else 0)});
                hasher.update(&.{@as(u8, if (a.returned) 1 else 0)});
            },
            .stack, .interned, .unset, .error_stub => {},
        }
    }

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
        // Get ptr instruction index - skip for globals/constants (no memory safety tracking needed)
        const ptr = switch (params.ptr) {
            .inst => |idx| idx,
            .interned, .fnptr => return, // globals/constants - no memory safety tracking
        };
        const src = switch (params.src) {
            .inst => |idx| idx,
            // comptime/interned values don't have memory safety tracking - skip
            .interned, .fnptr => return,
        };

        const src_refinement_idx = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_refinement_idx);

        // When storing a pointer with allocation tracking, set name_at_alloc on the pointee
        // This captures the access path (e.g., "container.ptr") for error messages
        if (src_refinement.* == .pointer) {
            // Get the pointee and check if it has allocation tracking
            const pointee_idx = src_refinement.pointer.to;
            const pointee = refinements.at(pointee_idx);
            if (getAnalytePtr(pointee).memory_safety) |*ms| {
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

        const param_meta = Meta{
            .function = ctx.meta.function,
            .file = ctx.meta.file,
            .line = ctx.base_line + 1,
            .column = null,
        };

        // Set parameter name on BOTH the pointer and pointee's memory_safety.
        // markAllocationsAsReturned checks the pointee's memory_safety for stack escape.
        if (tgt_ptr.analyte.memory_safety) |*ms| {
            if (ms.* == .stack) {
                ms.stack.name = .{ .parameter = param_name_id };
                ms.stack.meta = param_meta;
            }
        }

        // Also set on the pointee
        const pointee_idx = tgt_ptr.to;
        const pointee = refinements.at(pointee_idx);
        if (getAnalytePtr(pointee).memory_safety) |*ms| {
            if (ms.* == .stack) {
                ms.stack.name = .{ .parameter = param_name_id };
                ms.stack.meta = param_meta;
            }
        }
    }

    /// Retroactively set variable name on stack for escape detection messages.
    /// The name_id is already set on the instruction by DbgVarPtr.apply().
    /// Set name on BOTH the pointer and pointee's memory_safety - reportStackEscape
    /// checks the pointer's memory_safety.
    pub fn dbg_var_ptr(state: State, index: usize, params: tag.DbgVarPtrParams) !void {
        _ = index;
        const inst = params.ptr orelse return;
        const ptr_idx = state.results[inst].refinement orelse return;
        const ptr_ref = state.refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        // Set name on the POINTER's memory_safety (used by reportStackEscape)
        if (ptr_ref.pointer.analyte.memory_safety) |*ms| {
            if (ms.* == .stack) {
                if (ms.stack.name == .other) {
                    ms.stack.name = .{ .variable = params.name_id };
                }
            }
        }

        // Also set on the pointee's memory_safety
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

    /// With unified args, memory safety state is already set up by srcSliceToGidSlice.
    /// For inst args, state was copied from caller. For int_const args, splatInitDefined
    /// sets the initial state. Global pointer tracking for int_const pointers would
    /// need to be added to srcSliceToGidSlice if needed.
    pub fn arg(state: State, index: usize, params: tag.Arg) !void {
        _ = state;
        _ = index;
        _ = params;
        // Nothing to do - state was already set by srcSliceToGidSlice
    }

    /// struct_field_ptr creates a pointer to a field of a struct/union.
    /// Set root_gid on the POINTER to mark it as a derived pointer that cannot be freed directly.
    /// root_gid points to the CONTAINER so field_parent_ptr can recover the parent.
    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        const refinements = state.refinements;
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // Get container from base pointer - only handle instruction bases
        const base_idx: Gid = switch (params.base) {
            .inst => |inst| state.results[inst].refinement orelse {
                // No base refinement - set .unset on result and any nested refinements
                initUnsetRecursive(refinements, ptr_idx);
                return;
            },
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                initUnsetRecursive(refinements, ptr_idx);
                return;
            },
            .fnptr => {
                // Constant base - set .unset on result and nested refinements
                initUnsetRecursive(refinements, ptr_idx);
                return;
            },
        };
        const base_ref = refinements.at(base_idx);
        if (base_ref.* != .pointer) {
            initUnsetRecursive(refinements, ptr_idx);
            return;
        }

        const container_idx = base_ref.pointer.to;
        const container = refinements.at(container_idx);
        const container_analyte = getAnalytePtr(container);
        const container_ms = container_analyte.memory_safety orelse {
            initUnsetRecursive(refinements, ptr_idx);
            return;
        };

        // Create memory_safety for the pointer with root_gid pointing to container
        const container_gid = container.getGid();
        ptr.analyte.memory_safety = switch (container_ms) {
            .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = container_gid } },
            .allocated => |a| .{ .allocated = .{
                .meta = a.meta,
                .allocator_gid = a.allocator_gid,
                .type_id = a.type_id,
                .freed = a.freed,
                .root_gid = container_gid,
                .is_slice = a.is_slice,
            } },
            .interned => |g| .{ .interned = g },
            .unset => .{ .unset = {} },
            .error_stub => {
                initUnsetRecursive(refinements, ptr_idx);
                return;
            },
        };

        // Also ensure memory_safety is set on the pointee (the field).
        // This handles the case where the field was just created via typeToRefinement
        // (e.g., accessing an inactive union field for the first time).
        initUnsetRecursive(refinements, ptr.to);
    }

    /// slice_ptr extracts the pointer from a slice.
    /// The resulting pointer is a derived pointer - it cannot be freed directly.
    /// We set root_gid on the pointer to the region, so free detection knows it's derived.
    pub fn slice_ptr(state: State, index: usize, params: tag.SlicePtr) !void {
        const refinements = state.refinements;
        _ = params;

        // Get the result pointer
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // Get the region this pointer points to
        const region_idx = ptr.to;
        const region = refinements.at(region_idx);
        if (region.* != .region) {
            initUnsetRecursive(refinements, ptr_idx);
            return;
        }

        // Get the region's memory_safety - if none, set .unset on all (interned/static data)
        const region_ms = region.region.analyte.memory_safety orelse {
            // Interned slice (string literal, etc.) - set .unset on scalar, region, pointer
            initUnsetRecursive(refinements, ptr_idx);
            return;
        };

        // Create memory_safety for the pointer with root_gid pointing to the region
        const region_gid = region.getGid();
        ptr.analyte.memory_safety = switch (region_ms) {
            .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = region_gid } },
            .allocated => |a| .{ .allocated = .{
                .meta = a.meta,
                .allocator_gid = a.allocator_gid,
                .type_id = a.type_id,
                .freed = a.freed,
                .root_gid = region_gid,
                .is_slice = a.is_slice,
            } },
            .interned => |g| .{ .interned = g },
            .unset => .{ .unset = {} },
            .error_stub => .{ .unset = {} }, // error_stub shouldn't be on regions, set .unset
        };
    }

    /// ptr_add/ptr_sub performs pointer arithmetic.
    /// This is only valid on pointers to regions (many-item pointers, slices).
    /// Pointer arithmetic on single-item pointers (*T) is undefined behavior.
    pub fn ptr_add(state: State, index: usize, params: tag.PtrAdd) !void {
        _ = index;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const ptr_gid: Gid = switch (params.ptr) {
            .inst => |idx| state.results[idx].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // interned constant, can't track
        };
        const ptr_ref = refinements.at(ptr_gid);
        if (ptr_ref.* != .pointer) return;

        // Check what the pointer points to
        const pointee_ref = refinements.at(ptr_ref.pointer.to);
        if (pointee_ref.* != .region) {
            // Pointer arithmetic on non-region pointer (single-item pointer)
            try ctx.meta.print(ctx.writer, "pointer arithmetic on single-item pointer in ", .{});
            return error.PtrArithmeticOnSingleItem;
        }
    }

    /// ptr_sub has the same safety requirements as ptr_add
    pub const ptr_sub = ptr_add;

    /// field_parent_ptr recovers the parent container pointer from a field pointer.
    /// We use the stored root_gid to reconnect to the original container entity.
    pub fn field_parent_ptr(state: State, index: usize, params: tag.FieldParentPtr) !void {
        const refinements = state.refinements;

        // Initialize memory_safety on the result first. typeToRefinement creates
        // pointer -> struct -> fields, and even if we redirect the pointer to the
        // original parent below, the orphaned struct/field entities need memory_safety
        // set to pass testValid.
        if (state.results[index].refinement) |ref_idx| {
            initUnsetRecursive(refinements, ref_idx);
        }

        const ptr_idx: Gid = switch (params.field_ptr) {
            .inst => |idx| state.results[idx].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // interned constant, can't track
        };
        const ptr_ref = refinements.at(ptr_idx);
        // field_parent_ptr should only receive pointers from codegen
        if (ptr_ref.* != .pointer) std.debug.panic("field_parent_ptr: expected pointer, got {s}", .{@tagName(ptr_ref.*)});

        // Get parent GID from the field pointer's memory_safety.root_gid
        const ms = ptr_ref.pointer.analyte.memory_safety orelse return;
        const parent_gid: ?Gid = switch (ms) {
            .stack => |s| s.root_gid,
            .allocated => |a| a.root_gid,
            .interned => return, // Global pointer, no parent tracking
            .unset => return, // Unset pointer, can't recover parent
            .error_stub => @panic("field_parent_ptr: error_stub shouldn't be on pointers"),
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
                .allocator_gid = a.allocator_gid,
                .type_id = a.type_id,
                .freed = a.freed,
                .root_gid = null,
            } },
            .interned => |g| .{ .interned = g },
            .unset => .{ .unset = {} },
            .error_stub => @panic("field_parent_ptr: error_stub shouldn't be on parent containers"),
        };
    }

    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const src_idx: Gid = switch (params.src) {
            .inst => |idx| results[idx].refinement orelse return,
            // comptime/interned values don't have memory safety tracking - skip
            .interned, .fnptr => return,
        };

        // Recursively check for allocations to mark as returned
        try markAllocationsAsReturned(refinements, src_idx, ctx);
    }

    /// Recursively mark all allocations in a refinement tree as returned (not leaks).
    /// This handles pointers, optionals containing pointers, unions containing pointers, and structs.
    fn markAllocationsAsReturned(refinements: *Refinements, idx: Gid, ctx: *Context) !void {
        const refinement = refinements.at(idx);
        switch (refinement.*) {
            .pointer => |*p| {
                // Check pointee's memory_safety for stack pointer escape and allocation tracking.
                // A stack escape occurs when a pointer POINTS TO stack memory, not when the
                // pointer VALUE is on the stack. Loading a pointer from a struct field puts
                // the pointer value on the stack (.stack memory_safety), but if it points to
                // heap memory (.allocated), that's fine to return.
                const pointee_idx = p.to;
                const pointee = refinements.at(pointee_idx);
                if (getAnalytePtr(pointee).memory_safety) |*ms| {
                    if (ms.* == .stack) {
                        const sp = ms.stack;
                        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
                        if (std.mem.eql(u8, sp.meta.function, func_name)) {
                            return reportStackEscape(ms.*, ctx);
                        }
                    } else if (ms.* == .allocated) {
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
            .region => |r| try markAllocationsAsReturned(refinements, r.to, ctx),
            .recursive => |r| try markAllocationsAsReturned(refinements, r.to, ctx),
            .scalar, .allocator, .fnptr, .void, .noreturn, .unimplemented => {},
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
            .region => |r| clearAllocationsReturned(refinements, r.to),
            .recursive => |r| clearAllocationsReturned(refinements, r.to),
            .scalar, .fnptr, .void, .noreturn, .unimplemented, .allocator => {},
        }
    }

    /// Called after receiving a return value from a function call.
    /// Clears the "returned" flag on allocations to transfer ownership from callee to caller.
    pub fn call_return(refinements: *Refinements, return_gid: Gid) void {
        clearAllocationsReturned(refinements, return_gid);
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
                // For pointers, check if the POINTEE is stack memory from this function.
                // A pointer stored on the stack that points to heap memory is fine to return.
                // The escape is only if the pointer points to stack memory from this function.
                //
                // Check the pointee's memory_safety first.
                const pointee = refinements.at(p.to);
                if (getAnalytePtr(pointee).memory_safety) |ms| {
                    if (ms == .stack) {
                        const sp = ms.stack;
                        if (std.mem.eql(u8, sp.meta.function, func_name)) {
                            return reportStackEscape(ms, ctx);
                        }
                    }
                }
                // Also recurse into the pointee in case it contains nested pointers
                try checkStackEscapeRecursive(refinements, p.to, ctx, func_name);
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
            .region => |r| try checkStackEscapeRecursive(refinements, r.to, ctx, func_name),
            .recursive => |r| try checkStackEscapeRecursive(refinements, r.to, ctx, func_name),
            .scalar, .allocator, .fnptr, .void, .noreturn, .unimplemented => {},
        }
    }

    /// cond_br handler for error path detection.
    /// On error path (false branch of is_non_err), if the errorunion has error_stub,
    /// clear allocation metadata from the payload - it's a phantom allocation.
    pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        // Only care about false branch (error path)
        if (params.branch) return;

        // Get condition instruction
        const condition_idx = params.condition_idx orelse return;
        const cond_inst = results[condition_idx];

        // Check if condition is is_non_err
        const cond_tag = cond_inst.inst_tag orelse return;
        if (cond_tag != .is_non_err) return;

        // Get the errorunion being checked
        const src_inst = switch (cond_tag.is_non_err.src) {
            .inst => |i| i,
            else => return, // Comptime - no tracking needed
        };
        const eu_gid = results[src_inst].refinement orelse return;
        const eu_ref = refinements.at(eu_gid);
        if (eu_ref.* != .errorunion) return;

        // Check if errorunion has error_stub (allocation-derived)
        const ms = eu_ref.errorunion.analyte.memory_safety orelse return;
        if (ms != .error_stub) return;

        // On error path, the allocation didn't happen - clear allocation metadata from payload
        const payload_gid = eu_ref.errorunion.to;
        clearAllocationMetadata(refinements, payload_gid);
    }

    /// Recursively clear allocation metadata from a refinement and its children.
    /// Used on error path to mark phantom allocations as not actually allocated.
    /// Sets memory_safety to .unset (not null) to maintain testValid invariants.
    fn clearAllocationMetadata(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |*p| {
                p.analyte.memory_safety = .{ .unset = {} };
                clearAllocationMetadata(refinements, p.to);
            },
            .scalar => |*s| s.analyte.memory_safety = .{ .unset = {} },
            .@"struct" => |*st| {
                st.analyte.memory_safety = .{ .unset = {} };
                for (st.fields) |field_gid| {
                    clearAllocationMetadata(refinements, field_gid);
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = .{ .unset = {} };
                for (u.fields) |field_opt| {
                    if (field_opt) |field_gid| {
                        clearAllocationMetadata(refinements, field_gid);
                    }
                }
            },
            .region => |*r| {
                r.analyte.memory_safety = .{ .unset = {} };
                clearAllocationMetadata(refinements, r.to);
            },
            .optional => |*o| {
                o.analyte.memory_safety = .{ .unset = {} };
                clearAllocationMetadata(refinements, o.to);
            },
            .errorunion => |*e| {
                // errorunion uses error_stub to indicate it's a container
                e.analyte.memory_safety = .{ .error_stub = {} };
                clearAllocationMetadata(refinements, e.to);
            },
            .allocator => |*a| a.analyte.memory_safety = .{ .unset = {} },
            .fnptr => |*f| f.analyte.memory_safety = .{ .unset = {} },
            .recursive => |*r| {
                r.analyte.memory_safety = .{ .unset = {} };
                clearAllocationMetadata(refinements, r.to);
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Called on function close to check for memory leaks and stack pointer escapes.
    /// With global refinements, args share entities directly with caller.
    /// Stack pointer escapes through args are detected by checking arg pointees.
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements) !void {
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];

        // Check for stack pointer escapes via pointer arguments
        // If a pointer arg's pointee contains a stack pointer from this function,
        // it escapes to the caller via the shared global refinements table
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

        // Check for stack pointer escapes to global variables
        // If a global contains a stack pointer from this function, it escapes
        if (refinements.global_cutoff) |cutoff| {
            for (0..cutoff) |gid_usize| {
                const gid: Gid = @intCast(gid_usize);
                try checkStackEscapeRecursive(refinements, gid, ctx, func_name);
            }
        }

        // Build set of GIDs reachable from globals (allocations stored there aren't leaks)
        var global_reachable = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer global_reachable.deinit();
        if (refinements.global_cutoff) |cutoff| {
            for (0..cutoff) |gid_usize| {
                const gid: Gid = @intCast(gid_usize);
                collectReachableGids(refinements, gid, &global_reachable);
            }
        }

        // Check for memory leaks - allocation state is on the POINTEE
        // In main (stack depth 1), also check allocations stored in globals
        const in_main = ctx.stacktrace.items.len == 1;

        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            const refinement = refinements.at(idx);
            if (refinement.* != .pointer) continue;

            // Get the pointee entity via ptr.to
            const pointee_idx = refinement.pointer.to;
            const pointee = refinements.at(pointee_idx);

            // Only check data types that can have memory_safety for leaks.
            // Pointers CAN be allocated data (from `create(*T)`) so we check them too.
            switch (pointee.*) {
                .region, .scalar, .@"struct", .@"union", .pointer => {},
                else => continue,
            }

            const pointee_analyte = getAnalytePtr(pointee);

            // Check pointee's memory_safety for allocation state
            const ms = pointee_analyte.memory_safety orelse continue;
            if (ms != .allocated) continue;

            const allocation = ms.allocated;

            // Skip if freed, returned to caller, or (in non-main) reachable from a global
            // In main, even global-reachable allocations must be freed before program exit
            const skip_global_reachable = !in_main and global_reachable.contains(pointee_idx);

            // Check if allocator was deinited (for arena allocators)
            // The allocator GID is stable (semideepCopy returns same GID for allocators),
            // so we can directly check the allocator's deinit field.
            // allocator_gid always points to an allocator refinement
            const alloc_ref = refinements.at(allocation.allocator_gid).allocator;
            if (alloc_ref.deinit != null) {
                // Allocator was deinited - all its allocations are implicitly freed
                continue;
            }

            // Check for leak (allocation not freed, not returned, not global-reachable)
            if (allocation.freed == null and !allocation.returned and !skip_global_reachable) {
                // Report as arena leak if it was from an arena allocator
                // allocator_gid always points to an allocator refinement
                if (refinements.at(allocation.allocator_gid).allocator.arena_gid != null) {
                    return reportArenaLeak(ctx, allocation);
                }
                return reportMemoryLeak(ctx, allocation);
            }
        }

        // In main, also check allocations stored directly in global entities
        if (in_main) {
            if (refinements.global_cutoff) |cutoff| {
                for (0..cutoff) |gid_usize| {
                    const gid: Gid = @intCast(gid_usize);
                    try checkGlobalAllocationLeaks(refinements, gid, ctx);
                }
            }
        }
    }

    /// Check if a GID (or any reachable entity) contains an unfreed allocation
    fn checkGlobalAllocationLeaks(refinements: *Refinements, gid: Gid, ctx: *Context) !void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                // Check if the pointer points to an allocated entity
                const pointee = refinements.at(p.to);
                if (getAnalytePtr(pointee).memory_safety) |ms| {
                    if (ms == .allocated) {
                        const allocation = ms.allocated;
                        if (allocation.freed == null and !allocation.returned) {
                            return reportMemoryLeak(ctx, allocation);
                        }
                    }
                }
                // Recurse to nested pointers
                try checkGlobalAllocationLeaks(refinements, p.to, ctx);
            },
            .optional => |o| try checkGlobalAllocationLeaks(refinements, o.to, ctx),
            .errorunion => |e| try checkGlobalAllocationLeaks(refinements, e.to, ctx),
            .region => |r| try checkGlobalAllocationLeaks(refinements, r.to, ctx),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkGlobalAllocationLeaks(refinements, field_gid, ctx);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        try checkGlobalAllocationLeaks(refinements, field_gid, ctx);
                    }
                }
            },
            else => {},
        }
    }

    /// Recursively collect all GIDs reachable via .to fields from a starting GID.
    fn collectReachableGids(refinements: *Refinements, gid: Gid, reachable: *std.AutoHashMap(Gid, void)) void {
        // Already visited
        if (reachable.contains(gid)) return;
        reachable.put(gid, {}) catch return;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| collectReachableGids(refinements, p.to, reachable),
            .optional => |o| collectReachableGids(refinements, o.to, reachable),
            .errorunion => |e| collectReachableGids(refinements, e.to, reachable),
            .region => |r| collectReachableGids(refinements, r.to, reachable),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    collectReachableGids(refinements, field_gid, reachable);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        collectReachableGids(refinements, field_gid, reachable);
                    }
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // Allocation tracking (use-after-free, double-free, memory leak detection)
    // =========================================================================

    /// Get a mutable pointer to the Analyte for any refinement type that has one.
    /// Returns null for types without analytes (void, noreturn, unimplemented).
    fn getAnalytePtr(ref: *Refinements.Refinement) *Analyte {
        return switch (ref.*) {
            .scalar => |*s| &s.analyte,
            .pointer => |*p| &p.analyte,
            .optional => |*o| &o.analyte,
            .errorunion => |*e| &e.analyte,
            .@"struct" => |*st| &st.analyte,
            .@"union" => |*u| &u.analyte,
            .region => |*r| &r.analyte,
            .recursive => |*r| &r.analyte,
            .allocator => |*a| &a.analyte,
            .fnptr => |*f| &f.analyte,
            .void, .noreturn, .unimplemented => @panic("refinement type does not have analyte"),
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
                // Set memory_safety on the pointer and recurse into its .to
                // For stack allocations from typeToRefinement, .to is a placeholder
                // that may become orphaned when store updates it to the actual target.
                // We still need to set memory_safety on placeholders for testValid.
                p.analyte.memory_safety = ms;
                setStackRecursive(refinements, p.to, meta, child_root);
            },
            .region => |*r| {
                r.analyte.memory_safety = ms;
                setStackRecursive(refinements, r.to, meta, child_root);
            },
            .recursive => |*rec| {
                rec.analyte.memory_safety = ms;
                // Skip placeholders (.to == 0) that haven't been resolved yet
                if (rec.to != 0) {
                    setStackRecursive(refinements, rec.to, meta, child_root);
                }
            },
            .fnptr => |*f| {
                f.analyte.memory_safety = ms;
            },
            .allocator => |*a| {
                a.analyte.memory_safety = ms;
            },
            else => {},
        }
    }

    /// Base allocation info for setAllocatedRecursive (without root_gid, which is computed).
    const AllocatedBase = struct {
        meta: Meta,
        allocator_gid: Gid, // GID of allocator for identity comparison
        type_id: u32, // Type ID for error messages
        root_gid: ?Gid, // Initial root_gid for the root node
    };

    /// Recursively set .allocated memory_safety on all members of a refinement.
    /// Root gets root_gid from base (typically null), children get root_gid = root's gid.
    /// Pointers STOP recursion (they point to separate memory).
    fn setAllocatedRecursive(refinements: *Refinements, idx: Gid, base: AllocatedBase, root_gid: ?Gid, is_slice: bool) void {
        const ref = refinements.at(idx);
        const ms: MemorySafety = .{ .allocated = .{
            .meta = base.meta,
            .allocator_gid = base.allocator_gid,
            .type_id = base.type_id,
            .root_gid = root_gid,
            .is_slice = is_slice,
        } };
        // Children point to actual root (null for root, else the root's gid)
        const child_root = root_gid orelse ref.getGid();

        switch (ref.*) {
            .scalar => |*s| s.analyte.memory_safety = ms,
            .@"struct" => |*st| {
                st.analyte.memory_safety = ms;
                for (st.fields) |field_idx| {
                    setAllocatedRecursive(refinements, field_idx, base, child_root, is_slice);
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = ms;
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setAllocatedRecursive(refinements, field_idx, base, child_root, is_slice);
                    }
                }
            },
            .optional => |*o| {
                o.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, o.to, base, child_root, is_slice);
            },
            .errorunion => |*e| {
                e.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, e.to, base, child_root, is_slice);
            },
            .pointer => |*p| {
                // Set memory_safety on the pointer and recurse into its .to
                // For allocations from typeToRefinement, .to is a placeholder
                // that may become orphaned when store updates it to the actual target.
                // We still need to set memory_safety on placeholders for testValid.
                p.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, p.to, base, child_root, is_slice);
            },
            .region => |*r| {
                r.analyte.memory_safety = ms;
                setAllocatedRecursive(refinements, r.to, base, child_root, is_slice);
            },
            .recursive => |*rec| {
                rec.analyte.memory_safety = ms;
                // Skip placeholders (.to == 0) that haven't been resolved yet
                if (rec.to != 0) {
                    setAllocatedRecursive(refinements, rec.to, base, child_root, is_slice);
                }
            },
            .fnptr => |*f| {
                f.analyte.memory_safety = ms;
            },
            .allocator => |*a| {
                a.analyte.memory_safety = ms;
            },
            else => {},
        }
    }

    /// Recursively set .freed on all entities with .allocated memory_safety.
    /// This propagates the freed state to all struct/union fields.
    fn setFreedRecursive(refinements: *Refinements, idx: Gid, free_meta: Free) void {
        const ref = refinements.at(idx);

        // Set freed on this entity if it has .allocated
        if (getAnalytePtr(ref).memory_safety) |*ms| {
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
            .region => |r| setFreedRecursive(refinements, r.to, free_meta),
            else => {},
        }
    }

    /// Handle load - detect use-after-free.
    /// Checks the POINTEE's allocation state (via ptr.to) for freed status.
    pub fn load(state: State, index: usize, params: tag.Load) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Set result pointer's memory_safety to .stack (one deep only)
        // The loaded pointer VALUE is a copy living on the stack.
        // The POINTEE's memory_safety (via ptr.to) is unchanged - semideepCopy
        // shares the target, so allocated memory remains tracked as allocated.
        const result_idx = results[index].refinement orelse return;
        const result_ref = refinements.at(result_idx);
        if (result_ref.* == .pointer) {
            result_ref.pointer.analyte.memory_safety = .{ .stack = .{
                .meta = ctx.meta,
                .root_gid = null,
            } };
        }

        // Get ptr_idx from ptr - untracked interned loads have no memory safety tracking needed
        const ptr_idx: Gid = switch (params.ptr) {
            .inst => |ptr| results[ptr].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // No memory safety tracking for interned constants
        };
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

        // Check if allocation came from a deinited arena
        // allocator_gid always points to an allocator refinement
        const alloc_ref = refinements.at(ms.allocated.allocator_gid).allocator;
        if (alloc_ref.deinit) |deinit_meta| {
            return reportUseAfterArenaDeinit(ctx, ms.allocated, deinit_meta);
        }
    }

    /// Check a pointer operand for use-after-free (for memory operations)
    fn checkPtrUseAfterFree(state: State, src: tag.Src) !void {
        const refinements = state.refinements;
        const ctx = state.ctx;

        const ptr_gid: Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return,
        };

        const ptr_ref = refinements.at(ptr_gid);
        if (ptr_ref.* != .pointer) return;

        // Check the pointee - for slices this is a region
        const pointee_gid = ptr_ref.pointer.to;
        const pointee = refinements.at(pointee_gid);

        // Get memory_safety from either region or scalar
        const ms: ?MemorySafety = switch (pointee.*) {
            .region => |r| r.analyte.memory_safety,
            .scalar => |s| s.analyte.memory_safety,
            .@"struct" => |s| s.analyte.memory_safety,
            else => null,
        };

        if (ms == null) return;
        if (ms.? != .allocated) return;

        const allocated = ms.?.allocated;
        if (allocated.freed) |free_site| {
            return reportUseAfterFree(ctx, allocated, free_site);
        }

        // Check if allocation came from a deinited arena
        const alloc_ref = refinements.at(allocated.allocator_gid).allocator;
        if (alloc_ref.deinit) |deinit_meta| {
            return reportUseAfterArenaDeinit(ctx, allocated, deinit_meta);
        }
    }

    /// memcpy: copy from src to dest (noalias - no overlap allowed)
    pub fn memcpy(state: State, _: usize, params: tag.Memcpy) !void {
        try checkPtrUseAfterFree(state, params.dest);
        try checkPtrUseAfterFree(state, params.src);
    }

    /// memmove: copy from src to dest (overlap allowed)
    pub fn memmove(state: State, _: usize, params: tag.Memmove) !void {
        try checkPtrUseAfterFree(state, params.dest);
        try checkPtrUseAfterFree(state, params.src);
    }

    /// memset: set memory to value
    pub fn memset(state: State, _: usize, params: tag.Memset) !void {
        try checkPtrUseAfterFree(state, params.dest);
        // value is not a pointer, no use-after-free check needed
    }

    /// memset_safe: same as memset with safety checks
    pub const memset_safe = memset;

    /// Handle pointer/slice element pointer access - detect use-after-free for slices.
    /// For slices, the base is pointer → region → element.
    /// This handles both ptr_elem_ptr and slice_elem_ptr (uniform region model).
    pub fn ptr_elem_ptr(state: State, index: usize, params: tag.PtrElemPtr) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Set memory_safety on result pointer
        const result_ref_idx = results[index].refinement orelse return;
        const result_ref = refinements.at(result_ref_idx);
        if (result_ref.* != .pointer) return;

        const base_ref: Gid = switch (params.base) {
            .inst => |idx| results[idx].refinement orelse {
                // No base refinement - set result to unset
                result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
                return;
            },
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
                return;
            },
            .fnptr => {
                result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
                return;
            },
        };
        const base_refinement = refinements.at(base_ref).*;

        // For slices: base is pointer → region
        if (base_refinement != .pointer) {
            result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
            return;
        }
        const region_idx = base_refinement.pointer.to;
        const region_ref = refinements.at(region_idx);
        if (region_ref.* != .region) {
            result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
            return;
        }

        const ms = region_ref.region.analyte.memory_safety orelse {
            result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
            return;
        };

        // Copy memory_safety from region to result pointer with root_gid set
        switch (ms) {
            .allocated => |a| {
                result_ref.pointer.analyte.memory_safety = .{
                    .allocated = .{
                        .meta = a.meta,
                        .allocator_gid = a.allocator_gid,
                        .type_id = a.type_id,
                        .freed = a.freed,
                        .root_gid = region_idx, // derived pointer - can't be freed directly
                    },
                };
                if (a.freed) |free_site| {
                    return reportUseAfterFree(ctx, a, free_site);
                }
            },
            .stack => |s| {
                result_ref.pointer.analyte.memory_safety = .{ .stack = .{
                    .meta = s.meta,
                    .root_gid = region_idx,
                } };
            },
            .interned => |g| {
                result_ref.pointer.analyte.memory_safety = .{ .interned = g };
            },
            .unset => {
                result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
            },
            .error_stub => {
                result_ref.pointer.analyte.memory_safety = .{ .unset = {} };
            },
        }
    }

    /// Handle array/slice element value access - detect use-after-free for slices.
    /// For slices, the base is pointer → region → element.
    pub fn array_elem_val(state: State, index: usize, params: tag.ArrayElemVal) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const base_ref: Gid = switch (params.base) {
            .inst => |idx| results[idx].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // interned constant, can't track
        };
        const base_refinement = refinements.at(base_ref).*;

        // For slices: base is pointer → region
        if (base_refinement != .pointer) return;
        const region_idx = base_refinement.pointer.to;
        const region_ref = refinements.at(region_idx);
        if (region_ref.* != .region) return;

        const ms = region_ref.region.analyte.memory_safety orelse return;
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

    fn reportUseAfterArenaDeinit(ctx: *Context, allocation: Allocated, deinit_meta: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "use after free in ", .{});
        try deinit_meta.print(ctx.writer, "arena deinited in ", .{});
        // Use name_at_alloc for "allocated" line
        var buf: [256]u8 = undefined;
        const alloc_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
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

    fn reportArenaLeak(ctx: *Context, allocation: Allocated) anyerror {
        try ctx.meta.print(ctx.writer, "arena leak in ", .{});
        var buf: [256]u8 = undefined;
        const name_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
        if (name_prefix.len > 0) {
            try allocation.meta.print(ctx.writer, "{s}allocated in ", .{name_prefix});
        } else {
            try allocation.meta.print(ctx.writer, "allocated in ", .{});
        }
        return error.ArenaLeak;
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

    /// Check if two allocators match (same allocator instance).
    /// Handles both runtime allocators (with GIDs) and comptime allocators (with type_ids).
    fn allocatorsMatch(alloc_gid: ?Gid, alloc_type_id: u32, free_gid: ?Gid, free_type_id: u32) bool {
        if (alloc_gid != null and free_gid != null) {
            // Both runtime allocators - compare by GID
            return alloc_gid.? == free_gid.?;
        } else if (alloc_gid == null and free_gid == null) {
            // Both comptime allocators - compare by type_id
            return alloc_type_id == free_type_id;
        } else {
            // One runtime, one comptime - definitely different
            return false;
        }
    }

    /// Report mismatched allocation method (create vs alloc)
    /// alloc_is_slice: true if allocation was made with alloc (slice)
    /// free_is_slice: true if free was attempted with free (expects slice)
    fn reportMethodMismatch(ctx: *Context, allocation: Allocated, alloc_is_slice: bool, free_is_slice: bool) anyerror {
        try ctx.meta.print(ctx.writer, "allocation method mismatch in ", .{});
        const alloc_method = if (alloc_is_slice) "alloc" else "create";
        const free_method = if (free_is_slice) "free" else "destroy";
        try allocation.meta.print(ctx.writer, "allocated with ", .{});
        try ctx.writer.writeAll(alloc_method);
        try ctx.writer.writeAll(", freed with ");
        try ctx.writer.writeAll(free_method);
        try ctx.writer.writeAll("\n");
        return error.MethodMismatch;
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

    fn reportFreeGlobalMemory(ctx: *Context) anyerror {
        try ctx.meta.print(ctx.writer, "free of global/comptime memory in ", .{});
        return error.FreeGlobalMemory;
    }

    // =========================================================================
    // Branch merging
    // =========================================================================

    /// Merge memory_safety state from N branches for a single node.
    /// Called by tag.splatMerge which handles the tree traversal.
    ///
    /// For loops (merge_tag == .loop):
    /// - Index 0 in branches is the "null case" (loop didn't run / exit path)
    /// - Index 1+ are iteration states
    /// - We ignore index 0 when determining freed state - if ANY iteration freed, mark as freed
    /// - This prevents "loop body freed + exit path didn't free = not freed" false negatives
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        refinements: *Refinements,
        orig_gid: Gid,
        branches: []const ?State,
        branch_gids: []const ?Gid,
    ) !void {
        _ = ctx;
        const orig_ref = refinements.at(orig_gid);

        // Only pointer and scalar refinements have analytes
        const orig_analyte = switch (orig_ref.*) {
            .pointer => |*p| &p.analyte,
            .scalar => |*s| &s.analyte,
            else => return, // No memory_safety on container types
        };

        // Determine if this is a loop merge (where index 0 is the null case)
        const is_loop_merge = comptime merge_tag == .loop;

        // Handle allocation freed state merging
        // If original has an unfreed allocation, check if branches freed it
        if (orig_analyte.memory_safety) |*orig_ms| {
            switch (orig_ms.*) {
                .allocated => |*orig_alloc| {
                    if (orig_alloc.freed == null) {
                        // Both loop and normal branch merges use the same logic:
                        // require ALL paths to free. For loops, we only receive br_states
                        // (exit paths), so the semantics are the same as normal branches.
                        _ = is_loop_merge;
                        var all_freed = true;
                        var first_freed: ?Free = null;

                        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                            // Null branch = unreachable path (e.g., unreach in cold branch)
                            const branch = branch_opt orelse continue;
                            // Entity may not exist in all branches during recursive merge traversal
                            const branch_gid = branch_gid_opt orelse continue;
                            const branch_ref = branch.refinements.at(branch_gid);
                            // For optionals, branches may have different inner types
                            // (e.g., .null vs .scalar) - skip branches without analytes
                            const branch_analyte = switch (branch_ref.*) {
                                .pointer => |*p| &p.analyte,
                                .scalar => |*s| &s.analyte,
                                else => continue, // .null, .optional, etc. have no analyte
                            };
                            // Branch may have different memory_safety state (e.g., null branch has .unset)
                            // Only check freed state if branch also has .allocated
                            const branch_ms = branch_analyte.memory_safety orelse {
                                all_freed = false;
                                continue;
                            };
                            if (branch_ms != .allocated) {
                                all_freed = false;
                                continue;
                            }
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
                },
                .unset => {
                    // Original has .unset - copy from first branch that has allocation tracking
                    for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                        const branch = branch_opt orelse continue;
                        const branch_gid = branch_gid_opt orelse continue;
                        const branch_ref = branch.refinements.at(branch_gid);
                        // For optionals, branches may have different inner types
                        // (e.g., .null vs .scalar) - skip branches without analytes
                        const branch_analyte = switch (branch_ref.*) {
                            .pointer => |*p| &p.analyte,
                            .scalar => |*s| &s.analyte,
                            else => continue, // .null, .optional, etc. have no analyte
                        };
                        if (branch_analyte.memory_safety) |ms| {
                            if (ms == .allocated or ms == .interned) {
                                orig_analyte.memory_safety = ms;
                                break;
                            }
                        }
                    }
                },
                .stack => {}, // Stack pointers don't need merge handling
                .interned => {}, // Global pointers don't need merge handling
                .error_stub => {}, // error_stub only on errorunions, shouldn't appear here
            }
        } else {
            // Original has no memory_safety - copy from first branch that has it
            for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                // Null branch = unreachable path
                const branch = branch_opt orelse continue;
                // Entity may not exist in all branches during recursive merge traversal
                const branch_gid = branch_gid_opt orelse continue;
                const branch_ref = branch.refinements.at(branch_gid);
                // For optionals, branches may have different inner types
                // (e.g., .null vs .scalar) - skip branches without analytes
                const branch_analyte = switch (branch_ref.*) {
                    .pointer => |*p| &p.analyte,
                    .scalar => |*s| &s.analyte,
                    else => continue, // .null, .optional, etc. have no analyte
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

    /// Initialize memory_safety state on a return slot refinement.
    /// Pointees start as `.unset` - they get proper tracking when ret_safe fills the slot.
    pub fn retval_init(refinements: *Refinements, gid: Gid, ctx: *Context) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.memory_safety = .{ .unset = {} };
            },
            .pointer => |p| {
                ref.pointer.analyte.memory_safety = .{ .unset = {} };
                retval_init(refinements, p.to, ctx);
            },
            .optional => |o| {
                ref.optional.analyte.memory_safety = .{ .unset = {} };
                retval_init(refinements, o.to, ctx);
            },
            .errorunion => |e| {
                // errorunion gets error_stub - it's container metadata, not a tracked value
                ref.errorunion.analyte.memory_safety = .{ .error_stub = {} };
                retval_init(refinements, e.to, ctx);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = .{ .unset = {} };
                for (s.fields) |field_gid| {
                    retval_init(refinements, field_gid, ctx);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = .{ .unset = {} };
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        retval_init(refinements, field_gid, ctx);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.memory_safety = .{ .unset = {} };
            },
            .fnptr => {
                ref.fnptr.analyte.memory_safety = .{ .unset = {} };
            },
            .void => {},
            .region => |r| {
                ref.region.analyte.memory_safety = .{ .unset = {} };
                retval_init(refinements, r.to, ctx);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = .{ .unset = {} };
                retval_init(refinements, r.to, ctx);
            },
            .unimplemented => @panic("retval_init: unimplemented return type"),
            .noreturn => unreachable, // noreturn functions don't return, shouldn't reach here
        }
    }

    /// Initialize memory_safety to .unset for return values.
    /// We don't mark them as .interned because returned pointers may point to
    /// dynamically allocated memory (e.g., allocatedSlice returns a slice
    /// to allocated memory, not global memory).
    pub fn retval_init_defined(refinements: *Refinements, gid: Gid) void {
        retval_init_defined_inner(refinements, gid);
    }

    fn retval_init_defined_inner(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                ref.pointer.analyte.memory_safety = .{ .unset = {} };
                // Don't mark pointee as interned - returned pointers might point to
                // dynamically allocated memory (e.g., allocatedSlice), not just interned data.
                // Only interned/comptime pointers should have .interned memory_safety.
                retval_init_defined_inner(refinements, p.to);
            },
            .scalar => ref.scalar.analyte.memory_safety = .{ .unset = {} },
            .optional => |o| {
                ref.optional.analyte.memory_safety = .{ .unset = {} };
                retval_init_defined_inner(refinements, o.to);
            },
            .errorunion => |e| {
                ref.errorunion.analyte.memory_safety = .{ .error_stub = {} };
                retval_init_defined_inner(refinements, e.to);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = .{ .unset = {} };
                for (s.fields) |field_gid| {
                    retval_init_defined_inner(refinements, field_gid);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = .{ .unset = {} };
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        retval_init_defined_inner(refinements, field_gid);
                    }
                }
            },
            .allocator => ref.allocator.analyte.memory_safety = .{ .unset = {} },
            .fnptr => ref.fnptr.analyte.memory_safety = .{ .unset = {} },
            .region => |r| {
                ref.region.analyte.memory_safety = .{ .unset = {} };
                retval_init_defined_inner(refinements, r.to);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = .{ .unset = {} };
                retval_init_defined_inner(refinements, r.to);
            },
            .void, .noreturn => {},
            .unimplemented => {}, // unimplemented types are silently skipped
        }
    }

    /// Initialize memory_safety for comptime/interned values.
    /// These are compile-time constants and should have .interned memory_safety.
    /// This is called when creating entities for interned Src values that are
    /// not tracked globals (e.g., &global_value pointer constants).
    pub fn interned_init(refinements: *Refinements, gid: Gid) void {
        interned_init_inner(refinements, gid);
    }

    fn interned_init_inner(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                // Interned pointers are comptime constants - mark as interned
                ref.pointer.analyte.memory_safety = .{ .interned = comptime_interned_meta };
                // Pointee is also interned
                interned_init_inner(refinements, p.to);
            },
            .scalar => ref.scalar.analyte.memory_safety = .{ .interned = comptime_interned_meta },
            .optional => |o| {
                ref.optional.analyte.memory_safety = .{ .interned = comptime_interned_meta };
                interned_init_inner(refinements, o.to);
            },
            .errorunion => |e| {
                ref.errorunion.analyte.memory_safety = .{ .interned = comptime_interned_meta };
                interned_init_inner(refinements, e.to);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = .{ .interned = comptime_interned_meta };
                for (s.fields) |field_gid| {
                    interned_init_inner(refinements, field_gid);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = .{ .interned = comptime_interned_meta };
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        interned_init_inner(refinements, field_gid);
                    }
                }
            },
            .fnptr, .allocator => {},
            .region => |r| {
                ref.region.analyte.memory_safety = .{ .interned = comptime_interned_meta };
                interned_init_inner(refinements, r.to);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = .{ .interned = comptime_interned_meta };
                interned_init_inner(refinements, r.to);
            },
            .void, .noreturn => {},
            .unimplemented => {},
        }
    }

    /// Dummy Meta for comptime pointer constants (no real source location)
    const comptime_interned_meta = Meta{
        .function = "",
        .file = "<comptime>",
        .line = 0,
        .column = null,
    };

    // =========================================================================
    // Simple operation handlers - set memory_safety to .unset for computed values
    // =========================================================================

    /// Helper to set memory_safety = .unset on a scalar result
    fn setResultUnset(state: State, index: usize) void {
        const ref_idx = state.results[index].refinement orelse return;
        state.refinements.at(ref_idx).scalar.analyte.memory_safety = .{ .unset = {} };
    }

    /// Helper to set memory_safety = .unset on a struct and its fields
    fn setStructUnset(state: State, index: usize) void {
        const ref_idx = state.results[index].refinement orelse return;
        const ref = state.refinements.at(ref_idx);
        ref.@"struct".analyte.memory_safety = .{ .unset = {} };
        for (ref.@"struct".fields) |field_gid| {
            state.refinements.at(field_gid).scalar.analyte.memory_safety = .{ .unset = {} };
        }
    }

    // Simple arithmetic/comparison operations - produce computed scalars
    pub fn bit_and(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn cmp_eq(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn cmp_gt(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn cmp_gte(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn cmp_lt(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn cmp_lte(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn ctz(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn sub(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn add(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn slice_len(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn get_union_tag(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn is_non_null(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn is_null(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn is_non_null_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn is_null_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn is_non_err(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    /// optional_payload_ptr creates a pointer to the payload.
    /// The pointer has no allocation tracking (.unset).
    pub fn optional_payload_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ptr_idx = state.results[index].refinement orelse return;
        state.refinements.at(ptr_idx).pointer.analyte.memory_safety = .unset;
    }

    /// unwrap_errunion_payload_ptr creates a pointer to the error union payload.
    /// The pointer has no allocation tracking (.unset).
    pub fn unwrap_errunion_payload_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ptr_idx = state.results[index].refinement orelse return;
        state.refinements.at(ptr_idx).pointer.analyte.memory_safety = .unset;
    }

    /// Handle ArenaAllocator.init() - creates an ArenaAllocator.
    /// Checks if the child allocator is from a deinited arena.
    pub fn arena_init(state: State, index: usize, params: tag.ArenaInit) !void {
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Check if child allocator is from a deinited arena
        if (params.child_allocator_inst) |child_inst| {
            if (state.results[child_inst].refinement) |child_gid| {
                const child_ref = refinements.at(child_gid);
                // The child allocator could be an .allocator refinement
                if (child_ref.* == .allocator) {
                    // Check if this allocator was deinited
                    if (child_ref.allocator.deinit) |deinit_meta| {
                        return reportAllocAfterDeinit(ctx, deinit_meta);
                    }
                }
            }
        }

        // Initialize memory_safety on the result (ArenaAllocator struct)
        if (state.results[index].refinement) |gid| {
            initUnsetRecursive(refinements, gid);
        }
    }

    /// Handle ArenaAllocator.deinit() - marks the arena as deinited.
    /// All allocations made via this arena will be considered freed during leak check.
    pub fn arena_deinit(state: State, index: usize, params: tag.ArenaDeinit) !void {
        _ = index;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Get the argument's refinement GID
        const arg_gid: Gid = blk: {
            if (params.arena_inst) |arena_idx| {
                break :blk state.results[arena_idx].refinement orelse return;
            }
            return;
        };
        const arg_ref = refinements.at(arg_gid);

        // Get the target arena struct GID to mark as deinited.
        // The arg may be either a pointer or a loaded struct.
        const target_arena_gid: Gid = blk: {
            if (arg_ref.* == .pointer) {
                // Direct pointer - use pointee GID as the arena identifier
                break :blk arg_ref.pointer.to;
            } else if (arg_ref.* == .@"struct") {
                // Loaded struct - we need to find any allocator whose arena_gid
                // points to a struct with the same type_id
                const target_type_id = arg_ref.@"struct".type_id;
                for (refinements.list.items) |*ref| {
                    if (ref.* == .allocator) {
                        if (ref.allocator.arena_gid) |arena_struct_gid| {
                            const arena_ref = refinements.at(arena_struct_gid);
                            if (arena_ref.* == .@"struct" and arena_ref.@"struct".type_id == target_type_id) {
                                break :blk arena_struct_gid;
                            }
                        }
                    }
                }
                return; // No matching arena found
            } else {
                return;
            }
        };

        // Check for double-deinit by looking for any allocator already deinited
        for (refinements.list.items) |*ref| {
            if (ref.* == .allocator) {
                if (ref.allocator.arena_gid) |arena_struct_gid| {
                    if (arena_struct_gid == target_arena_gid) {
                        if (ref.allocator.deinit) |first_deinit| {
                            return reportDoubleDeinit(ctx, first_deinit);
                        }
                    }
                }
            }
        }

        // Mark ALL allocators that reference this arena as deinited.
        // This handles the case where arena.allocator() is called multiple times,
        // creating multiple allocator refinements that all point to the same arena.
        for (refinements.list.items) |*ref| {
            if (ref.* == .allocator) {
                if (ref.allocator.arena_gid) |arena_struct_gid| {
                    if (arena_struct_gid == target_arena_gid) {
                        ref.allocator.deinit = ctx.meta;
                    }
                }
            }
        }
    }

    /// Find the AllocatorRef refinement that references the given arena struct GID.
    fn findAllocatorForArena(refinements: *Refinements, arena_struct_gid: Gid) ?*Refinements.Refinement.AllocatorRef {
        for (refinements.list.items) |*ref| {
            if (ref.* == .allocator) {
                if (ref.allocator.arena_gid) |agid| {
                    if (agid == arena_struct_gid) {
                        return &ref.allocator;
                    }
                }
            }
        }
        return null;
    }

    fn reportDoubleDeinit(ctx: *Context, first_deinit: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "double arena deinit in ", .{});
        try first_deinit.print(ctx.writer, "previously deinited in ", .{});
        return error.DoubleDeinit;
    }

    fn reportAllocAfterDeinit(ctx: *Context, deinit_meta: Meta) anyerror {
        try ctx.meta.print(ctx.writer, "allocation from deinited allocator in ", .{});
        try deinit_meta.print(ctx.writer, "allocator deinited in ", .{});
        return error.AllocAfterDeinit;
    }

    // Overflow operations produce struct { result, overflow_flag }
    pub fn add_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        setStructUnset(state, index);
    }

    pub fn sub_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        setStructUnset(state, index);
    }

    pub fn mul_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        setStructUnset(state, index);
    }

    /// array_to_slice converts array/many-pointer to slice.
    /// If the source has no memory_safety, set .unset on the created refinements.
    pub fn array_to_slice(state: State, index: usize, params: anytype) !void {
        _ = params;
        const refinements = state.refinements;

        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // If pointer already has memory_safety, nothing to do (shared from source)
        if (ptr.analyte.memory_safety != null) return;

        // Source had no memory_safety - set .unset on the fresh structure
        const region_idx = ptr.to;
        const region = refinements.at(region_idx);
        if (region.* != .region) return;

        const element_idx = region.region.to;
        refinements.at(element_idx).scalar.analyte.memory_safety = .{ .unset = {} };
        refinements.at(region_idx).region.analyte.memory_safety = .{ .unset = {} };
        ptr.analyte.memory_safety = .{ .unset = {} };
    }

    // Simple operations that produce scalar values
    pub fn ret_addr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn intcast(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    /// aggregate_init creates a struct/array from element values.
    /// Copy memory_safety state from each source element to the corresponding field.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy memory_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i >= params.elements.len) {
                        // No source element - initialize to unset
                        initUnsetRecursive(state.refinements, field_gid);
                    } else {
                        const src = params.elements[i];
                        copyMemorySafetyState(state, field_gid, src);
                    }
                }
            },
            .region => |r| {
                // For arrays/regions: use uniform model - first element applies to all
                if (params.elements.len > 0) {
                    copyMemorySafetyState(state, r.to, params.elements[0]);
                } else {
                    initUnsetRecursive(state.refinements, r.to);
                }
            },
            else => {
                // Fallback: initialize to unset
                initUnsetRecursive(state.refinements, result_gid);
            },
        }
    }

    /// Copy memory_safety state from a source to a destination refinement.
    fn copyMemorySafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values have no allocation tracking
            .fnptr => null, // Function pointers have no allocation tracking
        };

        // For interned/fnptr sources, initialize destination to unset
        if (src_gid == null) {
            initUnsetRecursive(state.refinements, dst_gid);
            return;
        }

        // Copy analyte state recursively from source to destination
        copyMemorySafetyStateRecursive(state.refinements, dst_gid, src_gid.?);
    }

    /// Recursively copy memory_safety state from source GID to destination GID.
    fn copyMemorySafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        // Copy memory_safety analyte based on type
        switch (dst_ref.*) {
            .scalar => |*s| {
                s.analyte.memory_safety = switch (src_ref.*) {
                    .scalar => |ss| ss.analyte.memory_safety orelse .{ .unset = {} },
                    else => .{ .unset = {} },
                };
            },
            .pointer => |*p| {
                p.analyte.memory_safety = switch (src_ref.*) {
                    .pointer => |sp| sp.analyte.memory_safety orelse .{ .unset = {} },
                    else => .{ .unset = {} },
                };
                // Also copy pointee state
                if (src_ref.* == .pointer) {
                    copyMemorySafetyStateRecursive(refinements, p.to, src_ref.pointer.to);
                } else {
                    initUnsetRecursive(refinements, p.to);
                }
            },
            .optional => |*o| {
                o.analyte.memory_safety = switch (src_ref.*) {
                    .optional => |so| so.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .optional) {
                    copyMemorySafetyStateRecursive(refinements, o.to, src_ref.optional.to);
                } else {
                    initUnsetRecursive(refinements, o.to);
                }
            },
            .errorunion => |*e| {
                e.analyte.memory_safety = switch (src_ref.*) {
                    .errorunion => |se| se.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .errorunion) {
                    copyMemorySafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
                } else {
                    initUnsetRecursive(refinements, e.to);
                }
            },
            .@"struct" => |*s| {
                s.analyte.memory_safety = switch (src_ref.*) {
                    .@"struct" => |ss| ss.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .@"struct") {
                    const src_s = src_ref.@"struct";
                    for (s.fields, 0..) |field_gid, i| {
                        if (i < src_s.fields.len) {
                            copyMemorySafetyStateRecursive(refinements, field_gid, src_s.fields[i]);
                        } else {
                            initUnsetRecursive(refinements, field_gid);
                        }
                    }
                } else {
                    for (s.fields) |field_gid| {
                        initUnsetRecursive(refinements, field_gid);
                    }
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = switch (src_ref.*) {
                    .@"union" => |su| su.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .@"union") {
                    const src_u = src_ref.@"union";
                    for (u.fields, 0..) |maybe_field, i| {
                        const field_gid = maybe_field orelse continue;
                        if (i < src_u.fields.len) {
                            if (src_u.fields[i]) |src_field| {
                                copyMemorySafetyStateRecursive(refinements, field_gid, src_field);
                            } else {
                                initUnsetRecursive(refinements, field_gid);
                            }
                        } else {
                            initUnsetRecursive(refinements, field_gid);
                        }
                    }
                } else {
                    for (u.fields) |maybe_field| {
                        const field_gid = maybe_field orelse continue;
                        initUnsetRecursive(refinements, field_gid);
                    }
                }
            },
            .allocator => |*a| {
                a.analyte.memory_safety = switch (src_ref.*) {
                    .allocator => |sa| sa.analyte.memory_safety,
                    else => null,
                };
            },
            .fnptr => |*f| {
                f.analyte.memory_safety = switch (src_ref.*) {
                    .fnptr => |sf| sf.analyte.memory_safety,
                    else => null,
                };
            },
            .region => |*r| {
                r.analyte.memory_safety = switch (src_ref.*) {
                    .region => |sr| sr.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .region) {
                    copyMemorySafetyStateRecursive(refinements, r.to, src_ref.region.to);
                } else {
                    initUnsetRecursive(refinements, r.to);
                }
            },
            .recursive => |*rec| {
                rec.analyte.memory_safety = switch (src_ref.*) {
                    .recursive => |sr| sr.analyte.memory_safety,
                    else => null,
                };
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    pub fn wrap_errunion_err(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    pub fn unwrap_errunion_err(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    pub fn slice(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    pub fn dbg_inline_block(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultUnset(state, index);
    }

    // =========================================================================
    // typeToRefinement initialization handlers
    // These set memory_safety on refinements created by typeToRefinement
    // =========================================================================

    /// Helper to recursively set memory_safety = .unset on any refinement.
    /// Used for refinements created by typeToRefinement which don't have memory_safety set.
    /// Made public for use by variant_safety.merge when copying union fields.
    pub fn initUnsetRecursive(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                if (ref.scalar.analyte.memory_safety == null) {
                    ref.scalar.analyte.memory_safety = .{ .unset = {} };
                }
            },
            .pointer => |p| {
                if (ref.pointer.analyte.memory_safety == null) {
                    ref.pointer.analyte.memory_safety = .{ .unset = {} };
                }
                initUnsetRecursive(refinements, p.to);
            },
            .optional => |o| {
                if (ref.optional.analyte.memory_safety == null) {
                    ref.optional.analyte.memory_safety = .{ .unset = {} };
                }
                initUnsetRecursive(refinements, o.to);
            },
            .errorunion => |e| {
                if (ref.errorunion.analyte.memory_safety == null) {
                    ref.errorunion.analyte.memory_safety = .{ .error_stub = {} };
                }
                initUnsetRecursive(refinements, e.to);
            },
            .@"struct" => |s| {
                if (ref.@"struct".analyte.memory_safety == null) {
                    ref.@"struct".analyte.memory_safety = .{ .unset = {} };
                }
                for (s.fields) |field_gid| {
                    initUnsetRecursive(refinements, field_gid);
                }
            },
            .@"union" => |u| {
                if (ref.@"union".analyte.memory_safety == null) {
                    ref.@"union".analyte.memory_safety = .{ .unset = {} };
                }
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        initUnsetRecursive(refinements, field_gid);
                    }
                }
            },
            .allocator => {
                if (ref.allocator.analyte.memory_safety == null) {
                    ref.allocator.analyte.memory_safety = .{ .unset = {} };
                }
            },
            .fnptr => {
                if (ref.fnptr.analyte.memory_safety == null) {
                    ref.fnptr.analyte.memory_safety = .{ .unset = {} };
                }
            },
            .region => |r| {
                if (ref.region.analyte.memory_safety == null) {
                    ref.region.analyte.memory_safety = .{ .unset = {} };
                }
                initUnsetRecursive(refinements, r.to);
            },
            .recursive => |r| {
                if (ref.recursive.analyte.memory_safety == null) {
                    ref.recursive.analyte.memory_safety = .{ .unset = {} };
                }
                // Recursive types with .to=0 are placeholders that haven't been resolved yet.
                // This can happen for complex recursive types during initial construction.
                // For now, skip recursion on placeholders - they'll be initialized when resolved.
                if (r.to != 0) {
                    initUnsetRecursive(refinements, r.to);
                }
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Block creates result via typeToRefinement - initialize memory_safety
    pub fn block(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// MkAllocator creates an allocator refinement - initialize memory_safety
    pub fn mkallocator(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        const ref = state.refinements.at(ref_idx);
        ref.allocator.analyte.memory_safety = .{ .unset = {} };
    }

    /// StructFieldVal may create refinement via typeToRefinement for interned/global
    /// structs or for inactive union fields - initialize memory_safety
    pub fn struct_field_val(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        // Only init if memory_safety is null (not already set by container)
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// UnionInit creates union entity and may create field via typeToRefinement
    pub fn union_init(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// Bitcast may create optional wrapper - initialize memory_safety
    pub fn bitcast(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// SetUnionTag creates new field entity via typeToRefinement
    pub fn set_union_tag(state: State, index: usize, params: tag.SetUnionTag) !void {
        _ = index;
        const field_index = params.field_index orelse return;
        const ptr_gid: Gid = switch (params.ptr) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned => |interned| state.refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return,
        };
        const union_gid = state.refinements.at(ptr_gid).pointer.to;
        const field_gid = state.refinements.at(union_gid).@"union".fields[field_index] orelse return;
        initUnsetRecursive(state.refinements, field_gid);
    }

    /// WrapErrunionPayload may create payload via typeToRefinement
    pub fn wrap_errunion_payload(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// WrapOptional creates an optional refinement - initialize memory_safety
    pub fn wrap_optional(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// RetPtr creates a return value entity via typeToRefinement for struct/union returns
    pub fn ret_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// ErrunionPayloadPtrSet creates a new pointer to the payload - initialize memory_safety
    pub fn errunion_payload_ptr_set(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        initUnsetRecursive(state.refinements, ref_idx);
    }

    /// Helper to recursively set memory_safety = .interned on a refinement.
    fn setInternedRecursive(refinements: *Refinements, gid: Gid, meta: Meta) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.memory_safety = .{ .interned = meta };
            },
            .pointer => |p| {
                ref.pointer.analyte.memory_safety = .{ .interned = meta };
                setInternedRecursive(refinements, p.to, meta);
            },
            .optional => |o| {
                ref.optional.analyte.memory_safety = .{ .interned = meta };
                setInternedRecursive(refinements, o.to, meta);
            },
            .errorunion => |e| setInternedRecursive(refinements, e.to, meta),
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = .{ .interned = meta };
                for (s.fields) |field_gid| {
                    setInternedRecursive(refinements, field_gid, meta);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = .{ .interned = meta };
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        setInternedRecursive(refinements, field_gid, meta);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.memory_safety = .{ .interned = meta };
            },
            .fnptr => {
                ref.fnptr.analyte.memory_safety = .{ .interned = meta };
            },
            .region => |r| {
                ref.region.analyte.memory_safety = .{ .interned = meta };
                setInternedRecursive(refinements, r.to, meta);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = .{ .interned = meta };
                // Skip placeholders (.to == 0) that haven't been resolved yet
                if (r.to != 0) {
                    setInternedRecursive(refinements, r.to, meta);
                }
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Initialize memory_safety on global variables.
    /// Sets .interned on the pointer and recursively on the pointee.
    pub fn init_global(
        refinements: *Refinements,
        ptr_gid: Gid,
        pointee_gid: Gid,
        ctx: *Context,
        is_undefined: bool,
        is_null_opt: bool,
        loc: tag.GlobalLocation,
        field_info: ?tag.GlobalFieldInfo,
    ) void {
        _ = ctx;
        _ = is_undefined; // Handled by undefined_safety.init_global
        _ = is_null_opt; // Handled by null_safety.init_global
        _ = field_info; // Handled by fieldparentptr_safety.init_global

        const meta = Meta{
            .function = "",
            .file = loc.file,
            .line = loc.line,
            .column = loc.column,
        };

        // Set .interned on the pointer itself
        const ptr_ref = refinements.at(ptr_gid);
        if (ptr_ref.* == .pointer) {
            ptr_ref.pointer.analyte.memory_safety = .{ .interned = meta };
        }

        // Set .interned recursively on the pointee
        setInternedRecursive(refinements, pointee_gid, meta);
    }

    // =========================================================================
    // Runtime Call Filter
    // =========================================================================
    //
    // Called for every function call to determine if this module should intercept.
    // Returns true if the call was handled (skip normal execution), false otherwise.
    // This replaces compile-time FQN shim dispatch with runtime pattern matching.
    // =========================================================================

    /// Runtime call filter for memory safety.
    /// Checks FQN patterns to intercept allocator/arena operations.
    /// Returns true if intercepted (handled), false to continue with normal execution.
    pub fn call(
        state: State,
        index: usize,
        return_type: tag.Type,
        args: []const tag.Src,
        fqn: []const u8,
    ) anyerror!bool {
        if (gates.isAllocatorCreate(fqn)) {
            try handleAllocCreate(state, index, args);
            return true;
        }

        if (gates.isAllocatorDestroy(fqn)) {
            try handleAllocDestroy(state, index, args);
            return true;
        }

        if (gates.isAllocatorAlloc(fqn)) {
            try handleAllocAlloc(state, index, args);
            return true;
        }

        // alignedAlloc is same as alloc, just different FQN pattern
        if (gates.isAllocatorAlignedAlloc(fqn)) {
            try handleAllocAlloc(state, index, args);
            return true;
        }

        if (gates.isAllocatorFree(fqn)) {
            try handleAllocFree(state, index, args);
            return true;
        }

        // resize returns bool (success/failure), produces scalar - no memory tracking needed
        if (gates.isAllocatorResize(fqn)) {
            setResultUnset(state, index);
            return true;
        }

        // realloc/remap: frees old slice, allocates new slice
        if (gates.isAllocatorRealloc(fqn) or gates.isAllocatorRemap(fqn)) {
            try handleAllocRealloc(state, index, args);
            return true;
        }

        // ArenaAllocator.init: creates arena, checks child allocator not deinited
        if (gates.isArenaInit(fqn)) {
            try handleArenaInit(state, index, args);
            return true;
        }

        // ArenaAllocator.deinit: marks arena and all its allocations as freed
        if (gates.isArenaDeinit(fqn)) {
            try handleArenaDeinit(state, index, args);
            return true;
        }

        // GeneralPurposeAllocator.deinit: skip analysis, stdlib uses safe @ptrCast patterns
        // that trigger false positives (pointer arithmetic on single-item after @ptrCast to [*])
        if (gates.isGpaDeinit(fqn)) {
            setResultUnset(state, index);
            return true;
        }

        // MkAllocator: call returns std.mem.Allocator
        // The allocator refinement is already created by typeToRefinement.
        // We just need to link it to the arena if this is ArenaAllocator.allocator().
        if (return_type == .allocator) {
            try handleMkAllocator(state, index, args, fqn);
            return true;
        }

        // Formatter functions - check tuple args for use-after-free
        if (gates.isFormatter(fqn)) {
            try handleFormatter(state, args);
            return true; // Intercept - stub is generated
        }

        return false;
    }

    /// Handle formatter functions - check all args recursively for use-after-free
    fn handleFormatter(state: State, args: []const tag.Src) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        for (args) |src| {
            const arg_gid: Gid = switch (src) {
                // Inst args must have refinements
                .inst => |inst| results[inst].refinement.?,
                // Interned args are comptime - no memory safety issues possible
                .interned => continue,
                // Function pointers don't have memory safety concerns
                .fnptr => continue,
            };
            try checkUseAfterFreeRecursive(refinements, arg_gid, ctx);
        }
    }

    /// Recursively check a refinement for use-after-free (for tuple/struct args)
    fn checkUseAfterFreeRecursive(refinements: *Refinements, gid: Gid, ctx: *Context) !void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                const ms = p.analyte.memory_safety orelse return;
                switch (ms) {
                    .allocated => |alloc_state| {
                        if (alloc_state.freed) |freed| {
                            return reportUseAfterFree(ctx, alloc_state, freed);
                        }
                    },
                    .stack, .interned, .unset, .error_stub => {},
                }
            },
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkUseAfterFreeRecursive(refinements, field_gid, ctx);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_gid_opt| {
                    if (field_gid_opt) |field_gid| {
                        try checkUseAfterFreeRecursive(refinements, field_gid, ctx);
                    }
                }
            },
            .optional => |o| try checkUseAfterFreeRecursive(refinements, o.to, ctx),
            .errorunion => |e| try checkUseAfterFreeRecursive(refinements, e.to, ctx),
            .region => |r| try checkUseAfterFreeRecursive(refinements, r.to, ctx),
            .scalar, .allocator, .fnptr, .void, .noreturn, .unimplemented, .recursive => {},
        }
    }

    /// Handle mem.Allocator.create calls.
    /// The return structure (errorunion→ptr→pointee) is already created by Inst.call.
    /// We set memory_safety on the structure to track the allocation.
    fn handleAllocCreate(state: State, index: usize, args: []const tag.Src) !void {
        // Result is errorunion -> ptr -> pointee (created by Inst.call via typeToRefinement)
        const eu_idx = state.results[index].refinement orelse return;
        const eu_ref = state.refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return; // Safety check
        const eu = &eu_ref.errorunion;
        const ptr_ref = eu.to;
        const ptr = &state.refinements.at(ptr_ref).pointer;
        const pointee_ref = ptr.to;

        // Get allocator GID, type_id, and arena_gid from the allocator argument
        var allocator_gid: ?Gid = null;
        var type_id: u32 = 0;
        var arena_gid: ?Gid = null;

        // Try to get allocator info from args[0] (the allocator)
        if (args.len > 0) {
            switch (args[0]) {
                .inst => |inst| {
                    if (state.results[inst].refinement) |arg_gid| {
                        const ref = state.refinements.at(arg_gid);
                        if (ref.* != .allocator) return;
                        allocator_gid = arg_gid;
                        type_id = ref.allocator.type_id;
                        if (ref.allocator.deinit) |deinit_meta| {
                            return reportAllocAfterDeinit(state.ctx, deinit_meta);
                        }
                        arena_gid = ref.allocator.arena_gid;
                    }
                },
                .interned => |interned| {
                    // Allocators must be registered as globals
                    if (interned.ty != .allocator) return;
                    // getGlobal returns a pointer GID, follow it to get the allocator
                    const ptr_gid = state.refinements.getGlobal(interned.ip_idx) orelse {
                        std.debug.panic("allocator_create: interned allocator ip_idx={d} not registered as global", .{interned.ip_idx});
                    };
                    const agid = state.refinements.at(ptr_gid).pointer.to;
                    const alloc_ref = state.refinements.at(agid).allocator;
                    allocator_gid = agid;
                    type_id = alloc_ref.type_id;
                    if (alloc_ref.deinit) |deinit_meta| {
                        return reportAllocAfterDeinit(state.ctx, deinit_meta);
                    }
                    arena_gid = alloc_ref.arena_gid;
                },
                .fnptr => {},
            }
        }

        // Check if allocating from a deinited arena
        if (arena_gid) |agid| {
            if (findAllocatorForArena(state.refinements, agid)) |alloc_ref| {
                if (alloc_ref.deinit) |deinit_meta| {
                    return reportAllocAfterDeinit(state.ctx, deinit_meta);
                }
            }
        }

        // If we reach here, allocator_gid must be set
        const agid = allocator_gid orelse return;

        const alloc_base: AllocatedBase = .{
            .meta = state.ctx.meta,
            .allocator_gid = agid,
            .type_id = type_id,
            .root_gid = null, // This is the root allocation
        };

        // Set error_stub on errorunion to mark this as allocation-derived errorunion
        eu.analyte.memory_safety = .{ .error_stub = {} };

        // Set allocation state recursively on the pointee
        // Note: We only set memory_safety on the POINTEE, not the pointer itself.
        // The pointer is a return value (register/stack), the pointee is on the heap.
        setAllocatedRecursive(state.refinements, pointee_ref, alloc_base, null, false);
    }

    /// Handle mem.Allocator.destroy calls.
    /// Marks the pointed-to memory as freed after checking for errors.
    fn handleAllocDestroy(state: State, index: usize, args: []const tag.Src) !void {
        _ = index; // destroy returns void, no need to set result
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // destroy signature: destroy(self, ptr) -> args[0]=allocator, args[1]=ptr
        if (args.len < 2) return;

        // Resolve ptr source to get the pointer's refinement index
        const ptr_idx: Gid = switch (args[1]) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                // Trying to free a pointer to a global variable
                return reportFreeGlobalMemory(ctx);
            },
            .fnptr => {
                // Trying to free an interned constant pointer (e.g., pointer to global)
                return reportFreeGlobalMemory(ctx);
            },
        };
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) return; // Safety check

        // Get the pointee entity via ptr.to
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee = refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Get memory_safety from pointee
        const ms = &(pointee_analyte.memory_safety orelse return);

        switch (ms.*) {
            .stack => |sp| {
                // Check if this is a field pointer (root_gid != null means it's a field)
                if (sp.root_gid != null) {
                    return reportFreeFieldPointerStack(ctx, sp);
                }
                return reportFreeStackMemory(ctx, sp);
            },
            .interned => return reportFreeGlobalMemory(ctx),
            .unset => return,
            .error_stub => return,
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

        // Get allocator GID and type_id for mismatch detection
        var destroy_allocator_gid: ?Gid = null;
        var destroy_type_id: u32 = 0;
        if (args.len > 0) {
            switch (args[0]) {
                .inst => |inst| {
                    if (results[inst].refinement) |arg_gid| {
                        const ref = refinements.at(arg_gid);
                        if (ref.* != .allocator) return;
                        destroy_allocator_gid = arg_gid;
                        destroy_type_id = ref.allocator.type_id;
                    }
                },
                .interned => |interned| {
                    if (interned.ty != .allocator) return;
                    // getGlobal returns a pointer GID, follow it to get the allocator
                    const ptr_gid = refinements.getGlobal(interned.ip_idx) orelse {
                        std.debug.panic("allocator_destroy: interned allocator ip_idx={d} not registered as global", .{interned.ip_idx});
                    };
                    const agid = refinements.at(ptr_gid).pointer.to;
                    const alloc_ref = refinements.at(agid).allocator;
                    destroy_allocator_gid = agid;
                    destroy_type_id = alloc_ref.type_id;
                },
                .fnptr => {},
            }
        }

        // Check for allocator mismatch (only if destroy allocator GID is known)
        if (destroy_allocator_gid) |dag| {
            if (a.allocator_gid != dag) {
                return reportMismatchedAllocator(ctx, a, destroy_type_id);
            }
        }

        // destroy expects single item (create), not slice (alloc)
        if (a.is_slice) {
            return reportMethodMismatch(ctx, a, true, false);
        }

        // Mark as freed using setFreedRecursive to propagate to all fields
        // Get the inst for path building (if available)
        const ptr_inst: ?usize = switch (args[1]) {
            .inst => |inst| inst,
            else => null,
        };
        const free_meta: Free = .{
            .meta = ctx.meta,
            .name_at_free = if (ptr_inst) |inst| ctx.buildPathName(results, refinements, inst) else null,
        };
        setFreedRecursive(refinements, pointee_idx, free_meta);
    }

    /// Handle mem.Allocator.alloc/dupe/dupeZ calls.
    /// The return structure (errorunion→ptr→region→element) is already created by Inst.call.
    /// We set memory_safety on the structure to track the slice allocation.
    fn handleAllocAlloc(state: State, index: usize, args: []const tag.Src) !void {
        // Result is errorunion -> pointer -> region -> element (created by Inst.call via typeToRefinement)
        const eu_idx = state.results[index].refinement orelse return;
        const eu_ref = state.refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return; // Safety check
        const eu = &eu_ref.errorunion;
        const ptr_ref = eu.to;
        const ptr_refinement = state.refinements.at(ptr_ref);
        if (ptr_refinement.* != .pointer) return; // Safety check
        const ptr = &ptr_refinement.pointer;
        const region_ref = ptr.to;
        const region_refinement = state.refinements.at(region_ref);
        if (region_refinement.* != .region) return; // Safety check
        const region = &region_refinement.region;
        const element_ref = region.to;

        // Get allocator GID, type_id, and arena_gid from the allocator argument
        var allocator_gid: ?Gid = null;
        var type_id: u32 = 0;
        var arena_gid: ?Gid = null;

        // Try to get allocator info from args[0] (the allocator)
        if (args.len > 0) {
            switch (args[0]) {
                .inst => |inst| {
                    if (state.results[inst].refinement) |arg_gid| {
                        const ref = state.refinements.at(arg_gid);
                        if (ref.* != .allocator) return;
                        allocator_gid = arg_gid;
                        type_id = ref.allocator.type_id;
                        if (ref.allocator.deinit) |deinit_meta| {
                            return reportAllocAfterDeinit(state.ctx, deinit_meta);
                        }
                        arena_gid = ref.allocator.arena_gid;
                    }
                },
                .interned => |interned| {
                    // Allocators must be registered as globals
                    if (interned.ty != .allocator) return;
                    // getGlobal returns a pointer GID, follow it to get the allocator
                    const ptr_gid = state.refinements.getGlobal(interned.ip_idx) orelse {
                        std.debug.panic("allocator_alloc: interned allocator ip_idx={d} not registered as global", .{interned.ip_idx});
                    };
                    const agid = state.refinements.at(ptr_gid).pointer.to;
                    const alloc_ref = state.refinements.at(agid).allocator;
                    allocator_gid = agid;
                    type_id = alloc_ref.type_id;
                    if (alloc_ref.deinit) |deinit_meta| {
                        return reportAllocAfterDeinit(state.ctx, deinit_meta);
                    }
                    arena_gid = alloc_ref.arena_gid;
                },
                .fnptr => {},
            }
        }

        // Check if allocating from a deinited arena
        if (arena_gid) |agid| {
            if (findAllocatorForArena(state.refinements, agid)) |alloc_ref| {
                if (alloc_ref.deinit) |deinit_meta| {
                    return reportAllocAfterDeinit(state.ctx, deinit_meta);
                }
            }
        }

        // If we reach here, allocator_gid must be set
        const agid = allocator_gid orelse return;

        const alloc_base: AllocatedBase = .{
            .meta = state.ctx.meta,
            .allocator_gid = agid,
            .type_id = type_id,
            .root_gid = null,
        };

        // Set error_stub on errorunion
        eu.analyte.memory_safety = .{ .error_stub = {} };

        // Note: We only set memory_safety on the REGION and ELEMENT, not the pointer itself.
        // The pointer is a return value (register/stack), the region/element are on the heap.

        // Set memory_safety on region
        region.analyte.memory_safety = .{
            .allocated = .{
                .meta = alloc_base.meta,
                .allocator_gid = alloc_base.allocator_gid,
                .type_id = type_id,
                .root_gid = null,
                .is_slice = true,
            },
        };

        // Set allocation state recursively on the element
        setAllocatedRecursive(state.refinements, element_ref, alloc_base, null, true);
    }

    /// Handle mem.Allocator.free calls.
    /// Marks the slice memory as freed after checking for errors.
    fn handleAllocFree(state: State, index: usize, args: []const tag.Src) !void {
        _ = index; // free returns void
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // free signature: free(self, slice) -> args[0]=allocator, args[1]=slice
        if (args.len < 2) return;

        // Resolve slice source to get the pointer's refinement index
        const ptr_idx: Gid = switch (args[1]) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                return reportFreeGlobalMemory(ctx);
            },
            .fnptr => {
                return reportFreeGlobalMemory(ctx);
            },
        };
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) return;

        // Check the POINTER's memory_safety first - derived pointers have root_gid set
        if (ptr_refinement.pointer.analyte.memory_safety) |ptr_ms| {
            switch (ptr_ms) {
                .allocated => |a| {
                    if (a.root_gid != null) {
                        return reportFreeFieldPointer(ctx, a);
                    }
                },
                .stack => |s| {
                    if (s.root_gid != null) {
                        return reportFreeFieldPointerStack(ctx, s);
                    }
                },
                .interned => return reportFreeGlobalMemory(ctx),
                .unset => {},
                .error_stub => return,
            }
        }

        // Get the pointee entity
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee_ref = refinements.at(pointee_idx);

        const pointee_analyte: *Analyte = switch (pointee_ref.*) {
            .region => |*r| &r.analyte,
            .scalar => |*s| &s.analyte,
            else => return,
        };

        const ms_ptr = &(pointee_analyte.memory_safety orelse {
            // memory_safety is null - region was not tracked (e.g., returned from function)
            // Silently skip - we can't verify allocation status
            return;
        });

        switch (ms_ptr.*) {
            .stack => |sp| return reportFreeStackMemory(ctx, sp),
            .interned => return reportFreeGlobalMemory(ctx),
            .unset => return,
            .error_stub => return,
            .allocated => {},
        }

        const a = ms_ptr.allocated;

        if (a.freed) |previous_free| {
            return reportDoubleFree(ctx, a, previous_free);
        }

        // Determine allocator GID and type_id from the free call
        var free_allocator_gid: ?Gid = null;
        var free_type_id: u32 = 0;
        switch (args[0]) {
            .inst => |inst| {
                if (results[inst].refinement) |arg_gid| {
                    const ref = refinements.at(arg_gid);
                    if (ref.* != .allocator) return;
                    free_allocator_gid = arg_gid;
                    free_type_id = ref.allocator.type_id;
                }
            },
            .interned => |interned| {
                if (interned.ty != .allocator) return;
                // getGlobal returns a pointer GID, follow it to get the allocator
                const ptr_gid = refinements.getGlobal(interned.ip_idx) orelse {
                    std.debug.panic("allocator_free: interned allocator ip_idx={d} not registered as global", .{interned.ip_idx});
                };
                const agid = refinements.at(ptr_gid).pointer.to;
                const alloc_ref = refinements.at(agid).allocator;
                free_allocator_gid = agid;
                free_type_id = alloc_ref.type_id;
            },
            .fnptr => {},
        }

        // Check for mismatched allocator (only if free allocator GID is known)
        if (free_allocator_gid) |fag| {
            if (a.allocator_gid != fag) {
                return reportMismatchedAllocator(ctx, a, free_type_id);
            }
        }

        // free expects slice (alloc), not single item (create)
        if (!a.is_slice) {
            return reportMethodMismatch(ctx, a, false, true);
        }

        // Mark as freed
        const slice_inst: ?usize = switch (args[1]) {
            .inst => |inst| inst,
            else => null,
        };
        const free_meta: Free = .{
            .meta = ctx.meta,
            .name_at_free = if (slice_inst) |inst| ctx.buildPathName(results, refinements, inst) else null,
        };
        setFreedRecursive(refinements, pointee_idx, free_meta);
    }

    /// Handle mem.Allocator.realloc/remap calls.
    /// Marks the old slice as freed and the new slice as allocated.
    fn handleAllocRealloc(state: State, index: usize, args: []const tag.Src) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // realloc signature: realloc(self, slice, new_len) -> args[0]=allocator, args[1]=slice, args[2]=len
        if (args.len < 2) return;

        // === 1. Mark old slice as freed ===

        // Resolve slice source to get the pointer's refinement index
        const old_ptr_idx: Gid = switch (args[1]) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                return reportFreeGlobalMemory(ctx);
            },
            .fnptr => {
                return reportFreeGlobalMemory(ctx);
            },
        };
        const old_ptr_refinement = refinements.at(old_ptr_idx);
        if (old_ptr_refinement.* != .pointer) return;

        // Get the old region entity
        const old_region_idx = old_ptr_refinement.pointer.to;
        const old_region_ref = refinements.at(old_region_idx);
        if (old_region_ref.* != .region) return;

        const old_region_analyte = &old_region_ref.region.analyte;

        // Get memory_safety from old region
        const old_ms = &(old_region_analyte.memory_safety orelse {
            // memory_safety is null - region was not tracked
            return;
        });

        // Check for invalid operations on old slice
        switch (old_ms.*) {
            .stack => |sp| {
                if (sp.root_gid != null) {
                    return reportFreeFieldPointerStack(ctx, sp);
                }
                return reportFreeStackMemory(ctx, sp);
            },
            .interned => return reportFreeGlobalMemory(ctx),
            .unset => return,
            .error_stub => return,
            .allocated => {},
        }

        const old_a = old_ms.allocated;

        // Check for field pointer
        if (old_a.root_gid != null) {
            return reportFreeFieldPointer(ctx, old_a);
        }

        // Check for double-free
        if (old_a.freed) |previous_free| {
            return reportDoubleFree(ctx, old_a, previous_free);
        }

        // Get allocator GID and type_id for mismatch detection
        var realloc_allocator_gid: ?Gid = null;
        var realloc_type_id: u32 = 0;
        switch (args[0]) {
            .inst => |inst| {
                if (results[inst].refinement) |arg_gid| {
                    const ref = refinements.at(arg_gid);
                    if (ref.* != .allocator) return;
                    realloc_allocator_gid = arg_gid;
                    realloc_type_id = ref.allocator.type_id;
                }
            },
            .interned => |interned| {
                // Allocators must be registered as globals
                if (interned.ty != .allocator) return;
                // getGlobal returns a pointer GID, follow it to get the allocator
                const ptr_gid = refinements.getGlobal(interned.ip_idx) orelse {
                    std.debug.panic("allocator_realloc: interned allocator ip_idx={d} not registered as global", .{interned.ip_idx});
                };
                const agid = refinements.at(ptr_gid).pointer.to;
                const alloc_ref = refinements.at(agid).allocator;
                realloc_allocator_gid = agid;
                realloc_type_id = alloc_ref.type_id;
            },
            .fnptr => {},
        }

        // If we reach here, realloc_allocator_gid must be set
        const rag = realloc_allocator_gid orelse return;

        // Check for mismatched allocator
        if (old_a.allocator_gid != rag) {
            return reportMismatchedAllocator(ctx, old_a, realloc_type_id);
        }

        // Mark old slice as freed
        const slice_inst: ?usize = switch (args[1]) {
            .inst => |inst| inst,
            else => null,
        };
        const free_meta: Free = .{
            .meta = ctx.meta,
            .name_at_free = if (slice_inst) |inst| ctx.buildPathName(results, refinements, inst) else null,
        };
        setFreedRecursive(refinements, old_region_idx, free_meta);

        // === 2. Mark new slice as allocated ===

        // Result is errorunion -> pointer -> region -> element
        const eu_idx = results[index].refinement orelse return;
        const eu_ref = refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return;
        const eu = &eu_ref.errorunion;
        const new_ptr_idx = eu.to;
        const new_ptr_ref = refinements.at(new_ptr_idx);
        if (new_ptr_ref.* != .pointer) return;
        const new_ptr = &new_ptr_ref.pointer;
        const new_region_idx = new_ptr.to;
        const new_region_ref = refinements.at(new_region_idx);
        if (new_region_ref.* != .region) return;
        const new_region = &new_region_ref.region;
        const new_element_ref = new_region.to;

        const alloc_base: AllocatedBase = .{
            .meta = ctx.meta,
            .allocator_gid = rag,
            .type_id = realloc_type_id,
            .root_gid = null,
        };

        // Set error_stub on errorunion
        eu.analyte.memory_safety = .{ .error_stub = {} };

        // Set memory_safety on pointer
        new_ptr.analyte.memory_safety = .{
            .allocated = .{
                .meta = alloc_base.meta,
                .allocator_gid = alloc_base.allocator_gid,
                .type_id = realloc_type_id,
                .root_gid = null,
                .is_slice = true,
            },
        };

        // Set memory_safety on region
        new_region.analyte.memory_safety = .{
            .allocated = .{
                .meta = alloc_base.meta,
                .allocator_gid = alloc_base.allocator_gid,
                .type_id = realloc_type_id,
                .root_gid = null,
                .is_slice = true,
            },
        };

        // Set allocation state recursively on the element
        setAllocatedRecursive(refinements, new_element_ref, alloc_base, null, true);
    }

    /// Handle ArenaAllocator.init() - creates an ArenaAllocator.
    /// Checks if the child allocator is from a deinited arena.
    fn handleArenaInit(state: State, index: usize, args: []const tag.Src) !void {
        const refinements = state.refinements;
        const ctx = state.ctx;

        // ArenaAllocator.init signature: init(child_allocator) -> args[0]=child_allocator
        // Check if child allocator is from a deinited arena
        if (args.len > 0) {
            const child_gid: ?Gid = switch (args[0]) {
                .inst => |idx| state.results[idx].refinement,
                .interned => |interned| refinements.getGlobal(interned.ip_idx),
                .fnptr => null, // compile-time allocators
            };

            if (child_gid) |gid| {
                const child_ref = refinements.at(gid);
                if (child_ref.* == .allocator) {
                    if (child_ref.allocator.deinit) |deinit_meta| {
                        return reportAllocAfterDeinit(ctx, deinit_meta);
                    }
                }
            }
        }

        // Initialize memory_safety on the result (ArenaAllocator struct)
        // Inst.call already created the refinement, we just re-initialize it
        if (state.results[index].refinement) |gid| {
            initUnsetRecursive(refinements, gid);
        }
    }

    /// Handle ArenaAllocator.deinit() - marks the arena as deinited.
    /// All allocations made via this arena will be considered freed during leak check.
    fn handleArenaDeinit(state: State, index: usize, args: []const tag.Src) !void {
        _ = index;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // ArenaAllocator.deinit signature: deinit(self: *ArenaAllocator) -> args[0]=arena
        if (args.len < 1) return;

        // Get the argument's refinement GID
        const arg_gid: ?Gid = switch (args[0]) {
            .inst => |idx| state.results[idx].refinement,
            .interned => |interned| refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        const gid = arg_gid orelse return;
        const arg_ref = refinements.at(gid);

        // Get the target arena_gid to mark as deinited.
        // The arg may be either:
        // 1. A pointer to the arena - use pointer's pointee GID
        // 2. A loaded struct - search for matching arena by type_id
        const target_arena_gid: Gid = blk: {
            if (arg_ref.* == .pointer) {
                // Direct pointer - use pointee GID as the arena identifier
                break :blk arg_ref.pointer.to;
            } else if (arg_ref.* == .@"struct") {
                // Loaded struct - we need to find any allocator whose arena_gid
                // points to a struct with the same type_id
                const target_type_id = arg_ref.@"struct".type_id;
                for (refinements.list.items) |*ref| {
                    if (ref.* == .allocator) {
                        if (ref.allocator.arena_gid) |arena_struct_gid| {
                            const arena_ref = refinements.at(arena_struct_gid);
                            if (arena_ref.* == .@"struct" and arena_ref.@"struct".type_id == target_type_id) {
                                break :blk arena_struct_gid;
                            }
                        }
                    }
                }
                return; // No matching arena found
            } else {
                return;
            }
        };

        // Check for double-deinit by looking for any allocator already deinited
        for (refinements.list.items) |*ref| {
            if (ref.* == .allocator) {
                if (ref.allocator.arena_gid) |arena_struct_gid| {
                    if (arena_struct_gid == target_arena_gid) {
                        if (ref.allocator.deinit) |first_deinit| {
                            return reportDoubleDeinit(ctx, first_deinit);
                        }
                    }
                }
            }
        }

        // Mark ALL allocators that reference this arena as deinited.
        // This handles the case where arena.allocator() is called multiple times,
        // creating multiple allocator refinements that all point to the same arena.
        for (refinements.list.items) |*ref| {
            if (ref.* == .allocator) {
                if (ref.allocator.arena_gid) |arena_struct_gid| {
                    if (arena_struct_gid == target_arena_gid) {
                        ref.allocator.deinit = ctx.meta;
                    }
                }
            }
        }
    }

    /// Handle MkAllocator: call returns std.mem.Allocator.
    /// The allocator refinement is already created by typeToRefinement in Inst.call.
    /// This handler links the allocator to an arena if it's from ArenaAllocator.allocator().
    fn handleMkAllocator(state: State, index: usize, args: []const tag.Src, fqn: []const u8) !void {
        const results = state.results;
        const refinements = state.refinements;

        // Get the allocator refinement that was created by Inst.call via typeToRefinement
        const alloc_idx = results[index].refinement orelse return;
        const alloc_ref = refinements.at(alloc_idx);
        if (alloc_ref.* != .allocator) return;

        // Check if this is ArenaAllocator.allocator() - need to link to arena
        if (!gates.isArenaAllocator(fqn)) return;

        // ArenaAllocator.allocator() has signature: allocator(self: *ArenaAllocator) -> Allocator
        // args[0] is the arena pointer (self parameter)
        if (args.len < 1) return;

        // Get the arena pointer's refinement GID
        const arena_ptr_gid: Gid = switch (args[0]) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return,
        };

        // Follow the pointer to get the arena struct (pointee)
        const ptr_ref = refinements.at(arena_ptr_gid);
        if (ptr_ref.* != .pointer) return;
        const arena_struct_gid = ptr_ref.pointer.to;

        // Store the arena STRUCT GID (pointee).
        // At deinit time, we match by type_id since semideepCopy creates new GIDs.
        alloc_ref.allocator.arena_gid = arena_struct_gid;
    }
};

// =========================================================================
// Validation
// =========================================================================

const debug = @import("builtin").mode == .Debug;

/// Validate that a refinement conforms to memory_safety tracking rules.
/// - Non-trivial types (scalar, pointer, struct, union, optional, region, recursive, allocator)
///   MUST have memory_safety set
/// - errorunion can ONLY have error_stub set (or null)
/// - Trivial types (void, unimplemented, noreturn): no analyte, no memory_safety
pub fn testValid(refinement: Refinements.Refinement, idx: usize) void {
    if (!debug) return;
    switch (refinement) {
        .scalar => |s| {
            if (s.analyte.memory_safety == null) {
                std.debug.panic("memory_safety must be set on scalars (gid={})", .{idx});
            }
        },
        inline .pointer, .optional, .region, .recursive => |data, t| {
            if (data.analyte.memory_safety == null) {
                std.debug.panic("memory_safety must be set on {s}", .{@tagName(t)});
            }
        },
        inline .@"struct", .@"union" => |data, t| {
            if (data.analyte.memory_safety == null) {
                std.debug.panic("memory_safety must be set on {s}", .{@tagName(t)});
            }
        },
        .errorunion => |e| {
            if (e.analyte.memory_safety) |ms| {
                if (ms != .error_stub) {
                    std.debug.panic("errorunion can only have error_stub memory_safety, got {s}", .{@tagName(ms)});
                }
            }
        },
        .allocator => |a| {
            if (a.analyte.memory_safety == null) {
                std.debug.panic("memory_safety must be set on allocators", .{});
            }
        },
        .fnptr => |f| {
            if (f.analyte.memory_safety == null) {
                std.debug.panic("memory_safety must be set on fnptrs", .{});
            }
        },
        // Trivial types - no analyte, no memory_safety tracking
        .void, .noreturn, .unimplemented => {},
    }
}
