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
    // NOTE: Whether this is a slice (alloc) or single-item (create) allocation is determined
    // by WHERE the Allocated metadata lives:
    // - On a region → slice allocation (from alloc)
    // - On an element (scalar/struct) with no region above → single-item (from create)
    // - On an element wrapped in a region (from bitcast) → single-item (region has no .allocated)
};

pub const MemorySafety = union(enum) {
    stack: Stack,
    interned: Meta, // Comptime/interned values (globals, string literals, etc.)
    allocated: Allocated,

    /// only allowed on error unions.
    /// marks errorunions from allocation instructions.
    /// On error path, these are "phantom" allocations that didn't actually happen.
    /// cond_br looks up the errorunion directly via is_non_err.src from inst_tag.
    error_stub: void,

    /// pointer's gid is a placeholder and the pointer's value is actually
    /// still undefined.
    placeholder: void,

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
            },
            .stack, .interned, .error_stub, .placeholder => {},
        }
    }

    pub fn alloc(state: State, index: usize, params: tag.Alloc) !void {
        _ = params;
        // Inst contains .pointer = Indirected, set memory_safety on pointer and pointee
        const ptr_idx = state.results[index].refinement.?;
        const stack_ms: MemorySafety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };

        // Use paintReturnSlotMemory to traverse all nested structures including pointer.to
        // since typeToRefinement creates the entire nested structure together.
        paintReturnSlotMemory(state.refinements, ptr_idx, stack_ms);
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
            // For interned sources, mark the immediate destination as .interned ONLY if
            // the destination doesn't already have memory_safety set. If the destination
            // is already marked as .stack (e.g., from alloc), preserve that - the stack
            // location is where the data lives, regardless of the initial value's type.
            .interned => |interned| {
                switch (interned.ty) {
                    .pointer, .@"struct" => {
                        const ptr_refinement_idx = results[ptr].refinement orelse return;
                        const ptr_ref = refinements.at(ptr_refinement_idx);
                        if (ptr_ref.* == .pointer) {
                            const dest_idx = ptr_ref.pointer.to;
                            const dest_ref = refinements.at(dest_idx);
                            const dest_analyte = getAnalytePtr(dest_ref);
                            // Only set .interned if no memory_safety is already set
                            if (dest_analyte.memory_safety == null) {
                                dest_analyte.memory_safety = .{ .interned = ctx.meta };
                            }
                        }
                    },
                    else => {},
                }
                return;
            },
            .fnptr => return,
        };

        const src_refinement_idx = results[src].refinement orelse return;
        const src_refinement = refinements.at(src_refinement_idx);

        const ptr_refinement_idx = results[ptr].refinement orelse return;
        const ptr_ref = refinements.at(ptr_refinement_idx);
        if (ptr_ref.* != .pointer) return;

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

        // For struct stores, copy memory_safety state from source fields to destination fields.
        // tag.Store updates .to references to share targets, but analyte state also needs copying.
        // Without this, destination fields keep their original memory_safety (e.g., .stack from alloc)
        // while pointing to source regions that may have different memory_safety (e.g., .allocated).
        const dest_gid = ptr_ref.pointer.to;
        const dest_ref = refinements.at(dest_gid);
        if (dest_ref.* == .@"struct" and src_refinement.* == .@"struct") {
            const dest_struct = dest_ref.@"struct";
            const src_struct = src_refinement.@"struct";
            for (dest_struct.fields, src_struct.fields) |dest_field_gid, src_field_gid| {
                copyMemorySafetyRecursive(refinements, dest_field_gid, src_field_gid);
            }
        }

        // NOTE: Allocation escape via in-out arguments is now handled by connectivity
        // tracking - allocations reachable from caller-owned memory are not detected
        // as local leaks.

        // If storing from a parameter, propagate the parameter name_id and location to the destination's stack_ptr
        // Get name_id from the arg tag (name_id on Inst is only set by dbg_var_ptr)
        const src_tag = results[src].inst_tag orelse return;
        const param_name_id = switch (src_tag) {
            .arg => |a| a.name_id,
            else => return,
        };
        // Get the target pointer refinement (created by arg for pointer parameters)
        const tgt_refinement_idx = ptr_refinement_idx;
        const tgt_ptr = &refinements.at(tgt_refinement_idx).pointer;

        const param_meta = Meta{
            .function = ctx.meta.function,
            .file = ctx.meta.file,
            .line = ctx.base_line + 1,
            .column = null,
        };

        // Set parameter name on BOTH the pointer and pointee's memory_safety.
        // checkReturnedStackEscape checks the pointee's memory_safety for stack escape.
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
    /// For inst args, state was copied from caller. For int_const args, splatInit
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
                // No base refinement - set .stack on result and any nested refinements
                paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
                return;
            },
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                // Interned global - set .interned on result
                interned_init(refinements, ptr_idx);
                return;
            },
            .fnptr => {
                // Constant base - set .interned on result and nested refinements
                interned_init(refinements, ptr_idx);
                return;
            },
        };
        const base_ref = refinements.at(base_idx);
        if (base_ref.* != .pointer) {
            paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
            return;
        }

        const container_idx = base_ref.pointer.to;
        const container = refinements.at(container_idx);
        const container_analyte = getAnalytePtr(container);
        // If container has no memory_safety yet, leave result unset too.
        // Store operations will set the correct memory_safety later.
        const container_ms = container_analyte.memory_safety orelse return;

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
            } },
            .interned => |g| .{ .interned = g },
            .error_stub => {
                paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
                return;
            },
            .placeholder => std.debug.panic("placeholder in struct_field_ptr - container not initialized", .{}),
        };

        // Also ensure memory_safety is set on the pointee (the field).
        // This handles the case where the field was just created via typeToRefinement
        // (e.g., accessing an inactive union field for the first time).
        // Match the container's memory_safety type.
        switch (container_ms) {
            .stack => paintSpatialMemory(refinements, ptr.to, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } }),
            .interned => |g| paintSpatialMemory(refinements, ptr.to, .{ .interned = g }),
            .allocated => {
                // For allocated containers, fields are part of the allocation
                // Leave unset - the allocation tracking handles this
            },
            .error_stub => {},
            .placeholder => {}, // Already panicked above
        }
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
            paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
            return;
        }

        // Get the region's memory_safety - if none, set .interned (static data)
        const region_ms = region.region.analyte.memory_safety orelse {
            // Interned slice (string literal, etc.) - set .interned
            interned_init(refinements, ptr_idx);
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
            } },
            .interned => |g| .{ .interned = g },
            .error_stub => .{ .stack = .{ .meta = state.ctx.meta, .root_gid = region_gid } },
            .placeholder => std.debug.panic("placeholder in slice_ptr - region not initialized", .{}),
        };
    }

    /// slice_field_ptr (ptr_slice_ptr_ptr, ptr_slice_len_ptr) extracts a pointer to a slice component.
    /// For ptr_slice_ptr_ptr: takes *[]T and returns *[*]T (pointer to the .ptr field)
    /// For ptr_slice_len_ptr: takes *[]T and returns *usize (pointer to the .len field)
    /// The result is a pointer INTO the slice, so memory_safety should be inherited from the
    /// source pointer (which points to the slice field in a struct).
    pub fn slice_field_ptr(state: State, index: usize, params: tag.SliceFieldPtr) !void {
        const refinements = state.refinements;

        // Get the result pointer
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // Get the source pointer (pointer to slice)
        const src_idx: Gid = switch (params.src) {
            .inst => |inst| state.results[inst].refinement orelse {
                paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
                return;
            },
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                interned_init(refinements, ptr_idx);
                return;
            },
            .fnptr => {
                interned_init(refinements, ptr_idx);
                return;
            },
        };
        const src_ref = refinements.at(src_idx);
        if (src_ref.* != .pointer) {
            paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
            return;
        }

        // The source pointer (e.g., from struct_field_ptr) has the memory_safety we need.
        // This tells us where the slice field lives (stack of which function, etc.).
        const src_ms = src_ref.pointer.analyte.memory_safety orelse {
            paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
            return;
        };

        // The result is a pointer to a component of the slice (ptr or len).
        // Inherit memory_safety from the source pointer with root_gid pointing to the source.
        const src_gid = src_ref.getGid();
        ptr.analyte.memory_safety = switch (src_ms) {
            .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = src_gid } },
            .allocated => |a| .{ .allocated = .{
                .meta = a.meta,
                .allocator_gid = a.allocator_gid,
                .type_id = a.type_id,
                .freed = a.freed,
                .root_gid = src_gid,
            } },
            .interned => |g| .{ .interned = g },
            .error_stub => .{ .stack = .{ .meta = state.ctx.meta, .root_gid = src_gid } },
            .placeholder => std.debug.panic("placeholder in slice_field_ptr - source not initialized", .{}),
        };
    }

    /// ptr_add/ptr_sub perform pointer arithmetic on an already-validated source pointer.
    /// tag.PtrAdd is responsible for ensuring the source points to a region.
    pub fn ptr_add(state: State, index: usize, params: tag.PtrAdd) !void {
        const refinements = state.refinements;

        const ptr_gid: Gid = switch (params.ptr) {
            .inst => |idx| state.results[idx].refinement orelse return,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // interned constant, can't track
        };
        const ptr_ref = refinements.at(ptr_gid);
        if (ptr_ref.* != .pointer) return;
        const result_gid = state.results[index].refinement orelse return;
        paintSpatialMemory(refinements, result_gid, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
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
            paintSpatialMemory(refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
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
            .interned => |meta| {
                // Global pointer - result should also be .interned
                if (state.results[index].refinement) |ref_idx| {
                    paintSpatialMemory(refinements, ref_idx, .{ .interned = meta });
                }
                return;
            },
            .error_stub => return,
            .placeholder => std.debug.panic("placeholder in field_parent_ptr - entity not initialized", .{}),
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
            .error_stub => return,
            .placeholder => std.debug.panic("placeholder in field_parent_ptr - parent not initialized", .{}),
        };
    }

    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Check for leaks at early return point BEFORE marking allocations as returned.
        // This detects allocations that will become orphaned by this return.
        // Note: function-end leaks are handled by onFinish.
        try checkEarlyReturnLeaks(state, params);

        const src_idx: Gid = switch (params.src) {
            .inst => |idx| results[idx].refinement orelse return,
            // comptime/interned values don't have memory safety tracking - skip
            .interned, .fnptr => return,
        };
        // Check for stack pointer escapes in the return value
        try checkReturnedStackEscape(refinements, src_idx, ctx);
    }

    /// Check for memory leaks at an early return point.
    /// Detects allocations made in this function that are not being returned
    /// and will become unreachable after the return.
    ///
    /// Key insight: Only check allocations that are REACHABLE from local variables
    /// (the results array), not all entities in the refinements table. Entities
    /// inside null optionals or error payloads aren't accessible and shouldn't
    /// be considered for leak detection.
    fn checkEarlyReturnLeaks(state: State, params: tag.RetSafe) !void {
        const refinements = state.refinements;
        const results = state.results;
        const ctx = state.ctx;

        // Collect GIDs reachable from the return value (these will be marked as returned)
        var returned_reachable = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer returned_reachable.deinit();
        var returned_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer returned_alloc_ids.deinit();

        switch (params.src) {
            .inst => |idx| {
                if (results[idx].refinement) |src_gid| {
                    collectReachableGids(refinements, src_gid, &returned_reachable);
                    collectReachableAllocations(refinements, src_gid, &returned_alloc_ids);
                }
            },
            // Interned/fnptr returns don't contain allocations
            .interned, .fnptr => {},
        }

        // Collect GIDs reachable from arguments (these belong to caller, not leaks)
        var arg_reachable = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer arg_reachable.deinit();
        var arg_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer arg_alloc_ids.deinit();

        for (results) |inst| {
            const any_tag = inst.inst_tag orelse continue;
            if (any_tag != .arg) continue;
            const arg_gid = inst.refinement orelse continue;
            collectReachableGids(refinements, arg_gid, &arg_reachable);
            collectReachableAllocations(refinements, arg_gid, &arg_alloc_ids);
        }

        // Collect GIDs reachable from globals (allocations stored there aren't leaks)
        var global_reachable = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer global_reachable.deinit();
        var global_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer global_alloc_ids.deinit();

        if (refinements.global_cutoff) |cutoff| {
            for (0..cutoff) |gid_usize| {
                const gid: Gid = @intCast(gid_usize);
                collectReachableGids(refinements, gid, &global_reachable);
                collectReachableAllocations(refinements, gid, &global_alloc_ids);
            }
        }

        // Iterate over local variables (results array) and check for unfreed allocations.
        // This ensures we only check reachable allocations, not ones inside null optionals.
        for (results) |inst| {
            const gid = inst.refinement orelse continue;
            const ref = refinements.at(gid);

            // Only check pointers - allocation state is on the pointee
            if (ref.* != .pointer) continue;

            const pointee_gid = ref.pointer.to;

            // Skip if pointee is reachable from return value (will be marked as returned)
            if (returned_reachable.contains(pointee_gid)) continue;

            // Skip if pointee is reachable from args (belongs to caller)
            if (arg_reachable.contains(pointee_gid)) continue;

            // Skip if pointee is reachable from globals (stored in global, not a local leak)
            if (global_reachable.contains(pointee_gid)) continue;

            // Check pointee for unfreed allocation
            const pointee = refinements.at(pointee_gid);
            const ms = getAnalytePtr(pointee).memory_safety orelse continue;
            if (ms != .allocated) continue;

            const allocation = ms.allocated;
            const alloc_id = allocation.allocator_gid;

            // Skip if already freed
            if (allocation.freed != null) continue;

            // Check if allocator was deinited (for arena allocators)
            const alloc_ref = refinements.at(allocation.allocator_gid).allocator;
            if (alloc_ref.deinit != null) continue;

            if (returned_alloc_ids.contains(alloc_id)) continue;
            if (arg_alloc_ids.contains(alloc_id)) continue;
            if (global_alloc_ids.contains(alloc_id)) continue;

            // Report leak - allocation is orphaned by this early return
            if (alloc_ref.arena_gid != null) {
                return reportArenaLeak(ctx, allocation);
            }
            return reportMemoryLeak(ctx, allocation);
        }
    }

    /// Recursively check for stack pointer escapes in a refinement tree being returned.
    /// This handles pointers, optionals containing pointers, unions containing pointers, and structs.
    /// Note: Allocation tracking uses connectivity - allocations reachable from returned
    /// values are automatically not detected as leaks (no explicit marking needed).
    fn checkReturnedStackEscape(refinements: *Refinements, idx: Gid, ctx: *Context) !void {
        const refinement = refinements.at(idx);
        switch (refinement.*) {
            .pointer => |*p| {
                // Check pointee's memory_safety for stack pointer escape.
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
                    }
                }
                // Also recurse into the pointee to check nested structures
                // (e.g., slices/regions containing pointers)
                try checkReturnedStackEscape(refinements, pointee_idx, ctx);
            },
            .optional => |o| {
                // Check inner for pointers
                try checkReturnedStackEscape(refinements, o.to, ctx);
            },
            .errorunion => |e| {
                // Check payload for pointers
                try checkReturnedStackEscape(refinements, e.to, ctx);
            },
            .@"struct" => |s| {
                // Check all fields for pointers
                for (s.fields) |field_idx| {
                    try checkReturnedStackEscape(refinements, field_idx, ctx);
                }
            },
            .@"union" => |u| {
                // Check all fields for pointers
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        try checkReturnedStackEscape(refinements, field_idx, ctx);
                    }
                }
            },
            .region => |r| try checkReturnedStackEscape(refinements, r.to, ctx),
            .recursive => |r| try checkReturnedStackEscape(refinements, r.to, ctx),
            .scalar, .allocator, .fnptr, .void, .noreturn, .unimplemented => {},
        }
    }

    /// Called after receiving a return value from a function call.
    /// With connectivity tracking, no action is needed - allocations reachable from
    /// the return value are automatically not detected as leaks by the caller.
    pub fn call_return(refinements: *Refinements, return_gid: Gid) void {
        _ = refinements;
        _ = return_gid;
    }

    /// Check ret_load for stack pointer escapes.
    /// ret_load returns a value through ret_ptr storage - used for large returns (structs, unions).
    /// Allocation tracking uses connectivity - allocations reachable from return value are not leaks.
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
        // Note: allocation tracking is handled via connectivity - allocations reachable
        // from the returned value are automatically not detected as leaks.
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
    /// On error path (false branch of is_non_err), if the errorunion payload carries
    /// allocation metadata, mark that payload subtree as phantom with .error_stub.
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

        const payload_gid = eu_ref.errorunion.to;
        const payload_ref = refinements.at(payload_gid);

        // Skip payloads that don't have analytes (void, etc.)
        switch (payload_ref.*) {
            .void, .noreturn, .unimplemented, .fnptr => return,
            else => {},
        }

        const payload_ms = getAnalytePtr(payload_ref).memory_safety orelse return;
        if (payload_ms != .allocated and payload_ms != .error_stub) return;

        // On error path, the allocation didn't happen - mark the payload as phantom.
        markPayloadErrorStub(refinements, payload_gid);
    }

    /// Recursively mark a payload subtree with .error_stub.
    /// `.error_stub` belongs on payload structure, not on errorunion wrappers.
    fn markPayloadErrorStub(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |*p| {
                p.analyte.memory_safety = .{ .error_stub = {} };
                markPayloadErrorStub(refinements, p.to);
            },
            .scalar => |*s| s.analyte.memory_safety = .{ .error_stub = {} },
            .@"struct" => |*st| {
                st.analyte.memory_safety = .{ .error_stub = {} };
                for (st.fields) |field_gid| {
                    markPayloadErrorStub(refinements, field_gid);
                }
            },
            .@"union" => |*u| {
                u.analyte.memory_safety = .{ .error_stub = {} };
                for (u.fields) |field_opt| {
                    if (field_opt) |field_gid| {
                        markPayloadErrorStub(refinements, field_gid);
                    }
                }
            },
            .region => |*r| {
                r.analyte.memory_safety = .{ .error_stub = {} };
                markPayloadErrorStub(refinements, r.to);
            },
            .optional => |*o| {
                o.analyte.memory_safety = .{ .error_stub = {} };
                markPayloadErrorStub(refinements, o.to);
            },
            .errorunion => |e| {
                markPayloadErrorStub(refinements, e.to);
            },
            .allocator => |*a| a.analyte.memory_safety = .{ .error_stub = {} },
            .fnptr => |*f| f.analyte.memory_safety = .{ .error_stub = {} },
            .recursive => |*r| {
                r.analyte.memory_safety = .{ .error_stub = {} };
                if (r.to != 0) markPayloadErrorStub(refinements, r.to);
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Called on function close to check for memory leaks and stack pointer escapes.
    /// With global refinements, args share entities directly with caller.
    /// Stack pointer escapes through args are detected by checking arg pointees.
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements, return_gid: Gid) !void {
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
        var global_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer global_alloc_ids.deinit();
        if (refinements.global_cutoff) |cutoff| {
            for (0..cutoff) |gid_usize| {
                const gid: Gid = @intCast(gid_usize);
                collectReachableGids(refinements, gid, &global_reachable);
                collectReachableAllocations(refinements, gid, &global_alloc_ids);
            }
        }

        // Build set of GIDs reachable from arguments (belong to caller, not leaks)
        var arg_reachable = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer arg_reachable.deinit();
        var arg_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer arg_alloc_ids.deinit();
        for (results) |inst| {
            const any_tag = inst.inst_tag orelse continue;
            if (any_tag != .arg) continue;
            const arg_gid = inst.refinement orelse continue;
            collectReachableGids(refinements, arg_gid, &arg_reachable);
            collectReachableAllocations(refinements, arg_gid, &arg_alloc_ids);
        }

        // Build set of allocation identities reachable from return value (transferred to caller, not leaks)
        // We use allocation identity (meta) instead of GID because ret_safe deep-copies values,
        // so the return_gid contains copied entities with different GIDs than the results table.
        var return_alloc_ids = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer return_alloc_ids.deinit();
        collectReachableAllocations(refinements, return_gid, &return_alloc_ids);

        // Check for memory leaks via splatOrphaned.
        // In main (stack depth 1), also check allocations stored in globals.
        const in_main = ctx.stacktrace.items.len == 1;

        // Build OrphanContext for function_end leak detection
        const orphan_ctx: core.OrphanContext = .{
            .function_end = .{
                .function_name = func_name,
                .meta = ctx.meta,
                .in_main = in_main,
            },
        };

        // Track which pointees we've already checked to avoid duplicate reports
        var checked_pointees = std.AutoHashMap(Gid, void).init(ctx.allocator);
        defer checked_pointees.deinit();

        for (results) |inst| {
            const idx = inst.refinement orelse continue;
            const refinement = refinements.at(idx);
            if (refinement.* != .pointer) continue;

            // Get the pointee entity via ptr.to
            const pointee_idx = refinement.pointer.to;

            // Skip if we've already checked this pointee
            if (checked_pointees.contains(pointee_idx)) continue;
            try checked_pointees.put(pointee_idx, {});

            // Skip if pointee is reachable from args (belongs to caller)
            if (arg_reachable.contains(pointee_idx)) continue;

            // Skip if this allocation is reachable from return value (transferred to caller)
            // Check by allocation identity since ret_safe deep-copies values
            const pointee = refinements.at(pointee_idx);

            // Skip pointees that don't have analytes (void, fnptr, etc.)
            // These can't have memory allocations to leak
            switch (pointee.*) {
                .void, .noreturn, .unimplemented, .fnptr => continue,
                else => {},
            }

            if (getAnalytePtrConst(pointee).memory_safety) |ms| {
                if (ms == .allocated) {
                    const alloc_id = ms.allocated.allocator_gid;
                    if (arg_alloc_ids.contains(alloc_id)) continue;
                    if (return_alloc_ids.contains(alloc_id)) continue;
                    if (!in_main and global_alloc_ids.contains(alloc_id)) continue;
                }
            }

            // Skip if pointee is global-reachable (unless in main)
            // In main, even global-reachable allocations must be freed before program exit
            if (!in_main and global_reachable.contains(pointee_idx)) continue;

            // splatOrphaned will check allocation state and report leaks
            try tag.splatOrphaned(ctx, refinements, refinements, idx, null, orphan_ctx);
        }

        // In main, also check allocations stored directly in global entities
        if (in_main) {
            if (refinements.global_cutoff) |cutoff| {
                for (0..cutoff) |gid_usize| {
                    const gid: Gid = @intCast(gid_usize);
                    try checkGlobalAllocationLeaks(refinements, gid, ctx, orphan_ctx, &checked_pointees);
                }
            }
        }
    }

    /// Check if a GID (or any reachable entity) contains an unfreed allocation.
    /// Uses splatOrphaned for leak detection.
    fn checkGlobalAllocationLeaks(
        refinements: *Refinements,
        gid: Gid,
        ctx: *Context,
        orphan_ctx: core.OrphanContext,
        checked_pointees: *std.AutoHashMap(Gid, void),
    ) !void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                // Skip if we've already checked this pointee
                if (!checked_pointees.contains(p.to)) {
                    try checked_pointees.put(p.to, {});
                    // Use splatOrphaned for this pointer
                    try tag.splatOrphaned(ctx, refinements, refinements, gid, null, orphan_ctx);
                }
                // Recurse to nested pointers
                try checkGlobalAllocationLeaks(refinements, p.to, ctx, orphan_ctx, checked_pointees);
            },
            .optional => |o| try checkGlobalAllocationLeaks(refinements, o.to, ctx, orphan_ctx, checked_pointees),
            .errorunion => |e| try checkGlobalAllocationLeaks(refinements, e.to, ctx, orphan_ctx, checked_pointees),
            .region => |r| try checkGlobalAllocationLeaks(refinements, r.to, ctx, orphan_ctx, checked_pointees),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkGlobalAllocationLeaks(refinements, field_gid, ctx, orphan_ctx, checked_pointees);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        try checkGlobalAllocationLeaks(refinements, field_gid, ctx, orphan_ctx, checked_pointees);
                    }
                }
            },
            else => {},
        }
    }

    /// Check if target_gid is reachable from source_gid by traversing refinement links.
    fn isReachableFrom(refinements: *Refinements, target_gid: Gid, source_gid: Gid) bool {
        return isReachableFromInner(refinements, target_gid, source_gid, 0);
    }

    fn isReachableFromInner(refinements: *Refinements, target_gid: Gid, current_gid: Gid, depth: usize) bool {
        // Prevent infinite recursion
        if (depth > 100) return false;

        if (current_gid == target_gid) return true;

        const ref = refinements.at(current_gid);
        return switch (ref.*) {
            .pointer => |p| blk: {
                if (p.analyte.memory_safety) |ms| {
                    const root_gid = switch (ms) {
                        .allocated => |a| a.root_gid,
                        .stack => |s| s.root_gid,
                        .interned, .error_stub => null,
                    };
                    if (root_gid) |root| {
                        if (isReachableFromInner(refinements, target_gid, root, depth + 1)) {
                            break :blk true;
                        }
                    }
                }
                break :blk isReachableFromInner(refinements, target_gid, p.to, depth + 1);
            },
            .optional => |o| isReachableFromInner(refinements, target_gid, o.to, depth + 1),
            .errorunion => |e| isReachableFromInner(refinements, target_gid, e.to, depth + 1),
            .region => |r| isReachableFromInner(refinements, target_gid, r.to, depth + 1),
            .@"struct" => |s| blk: {
                for (s.fields) |field_gid| {
                    if (isReachableFromInner(refinements, target_gid, field_gid, depth + 1)) {
                        break :blk true;
                    }
                }
                break :blk false;
            },
            .@"union" => |u| blk: {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        if (isReachableFromInner(refinements, target_gid, field_gid, depth + 1)) {
                            break :blk true;
                        }
                    }
                }
                break :blk false;
            },
            else => false,
        };
    }

    /// Recursively collect all GIDs reachable via .to fields from a starting GID.
    fn collectReachableGids(refinements: *Refinements, gid: Gid, reachable: *std.AutoHashMap(Gid, void)) void {
        // Already visited
        if (reachable.contains(gid)) return;
        reachable.put(gid, {}) catch return;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                if (p.analyte.memory_safety) |ms| {
                    const root_gid = switch (ms) {
                        .allocated => |a| a.root_gid,
                        .stack => |s| s.root_gid,
                        .interned, .error_stub, .placeholder => null,
                    };
                    if (root_gid) |root| {
                        collectReachableGids(refinements, root, reachable);
                    }
                }
                collectReachableGids(refinements, p.to, reachable);
            },
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

    /// Recursively collect allocator GIDs from reachable allocated values.
    fn collectReachableAllocations(refinements: *Refinements, gid: Gid, allocs: *std.AutoHashMap(Gid, void)) void {
        collectReachableAllocationsInner(refinements, gid, allocs, 0);
    }

    fn collectReachableAllocationsInner(
        refinements: *Refinements,
        gid: Gid,
        allocs: *std.AutoHashMap(Gid, void),
        depth: usize,
    ) void {
        if (depth > 100) return; // Prevent infinite recursion
        if (gid >= refinements.list.items.len) return;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                if (p.analyte.memory_safety) |ms| {
                    const root_gid = switch (ms) {
                        .allocated => |a| a.root_gid,
                        .stack => |s| s.root_gid,
                        .interned, .error_stub, .placeholder => null,
                    };
                    if (root_gid) |root| {
                        collectReachableAllocationsInner(refinements, root, allocs, depth + 1);
                    }
                }
                // Check pointee for allocation
                const pointee = refinements.at(p.to);
                const pointee_analyte = getAnalytePtrConst(pointee);
                if (pointee_analyte.memory_safety) |ms| {
                    if (ms == .allocated) {
                        allocs.put(ms.allocated.allocator_gid, {}) catch return;
                    }
                }
                collectReachableAllocationsInner(refinements, p.to, allocs, depth + 1);
            },
            .optional => |o| collectReachableAllocationsInner(refinements, o.to, allocs, depth + 1),
            .errorunion => |e| collectReachableAllocationsInner(refinements, e.to, allocs, depth + 1),
            .region => |r| collectReachableAllocationsInner(refinements, r.to, allocs, depth + 1),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    collectReachableAllocationsInner(refinements, field_gid, allocs, depth + 1);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        collectReachableAllocationsInner(refinements, field_gid, allocs, depth + 1);
                    }
                }
            },
            else => {},
        }
    }

    /// Check if a refinement at a given GID has an allocation using the given allocator.
    /// This is used during branch merge to detect if an allocation is still reachable
    /// through another entity (avoiding false positive leak reports).
    fn hasMatchingAllocation(refinements: *Refinements, gid: Gid, allocator_gid: Gid) bool {
        return hasMatchingAllocationInner(refinements, gid, allocator_gid, 0);
    }

    fn hasMatchingAllocationInner(refinements: *Refinements, gid: Gid, allocator_gid: Gid, depth: usize) bool {
        if (depth > 100) return false; // Prevent infinite recursion
        if (gid >= refinements.list.items.len) return false;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .pointer => |p| {
                if (p.analyte.memory_safety) |ms| {
                    const root_gid = switch (ms) {
                        .allocated => |a| a.root_gid,
                        .stack => |s| s.root_gid,
                        .interned, .error_stub, .placeholder => null,
                    };
                    if (root_gid) |root| {
                        if (hasMatchingAllocationInner(refinements, root, allocator_gid, depth + 1)) {
                            return true;
                        }
                    }
                }
                // Check pointee for allocation
                const pointee = refinements.at(p.to);
                const pointee_analyte = getAnalytePtrConst(pointee);
                if (pointee_analyte.memory_safety) |ms| {
                    if (ms == .allocated) {
                        if (ms.allocated.allocator_gid == allocator_gid) {
                            return true;
                        }
                    }
                }
                return hasMatchingAllocationInner(refinements, p.to, allocator_gid, depth + 1);
            },
            .optional => |o| return hasMatchingAllocationInner(refinements, o.to, allocator_gid, depth + 1),
            .errorunion => |e| return hasMatchingAllocationInner(refinements, e.to, allocator_gid, depth + 1),
            .region => |r| {
                // Check region's own memory_safety for allocation
                if (r.analyte.memory_safety) |ms| {
                    if (ms == .allocated) {
                        if (ms.allocated.allocator_gid == allocator_gid) {
                            return true;
                        }
                    }
                }
                // Also recurse into element type
                return hasMatchingAllocationInner(refinements, r.to, allocator_gid, depth + 1);
            },
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    if (hasMatchingAllocationInner(refinements, field_gid, allocator_gid, depth + 1)) {
                        return true;
                    }
                }
                return false;
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        if (hasMatchingAllocationInner(refinements, field_gid, allocator_gid, depth + 1)) {
                            return true;
                        }
                    }
                }
                return false;
            },
            else => return false,
        }
    }

    /// Get a const pointer to the Analyte for any refinement type that has one.
    fn getAnalytePtrConst(ref: *const Refinements.Refinement) *const Analyte {
        return switch (ref.*) {
            .scalar => |*s| &s.analyte,
            .pointer => |*p| &p.analyte,
            .optional => |*o| &o.analyte,
            .errorunion => |*e| &e.analyte,
            .@"struct" => |*st| &st.analyte,
            .region => |*r| &r.analyte,
            .recursive => |*rec| &rec.analyte,
            .@"union" => |*un| &un.analyte,
            .allocator => |*a| &a.analyte,
            .fnptr => |*f| &f.analyte,
            .void, .noreturn, .unimplemented => @panic("no analyte on this type"),
        };
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

    /// Base allocation info for setAllocatedRecursive (without root_gid, which is computed).
    const AllocatedBase = struct {
        meta: Meta,
        allocator_gid: Gid, // GID of allocator for identity comparison
        type_id: u32, // Type ID for error messages
        root_gid: ?Gid, // Initial root_gid for the root node
    };

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

        // Set result pointer's memory_safety based on source:
        // - Loading from interned global: result points to interned memory -> .interned
        // - Loading from local/inst ptr: result is a copy on the stack -> .stack
        const result_idx = results[index].refinement orelse return;
        const result_ref = refinements.at(result_idx);
        if (result_ref.* == .pointer) {
            switch (params.ptr) {
                .interned => {
                    // Loading from interned global - result points to interned memory
                    paintSpatialMemory(refinements, result_idx, .{ .interned = ctx.meta });
                },
                .inst, .fnptr => {
                    // Loading from local pointer - result is a stack copy
                    result_ref.pointer.analyte.memory_safety = .{ .stack = .{
                        .meta = ctx.meta,
                        .root_gid = null,
                    } };
                },
            }
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
                // No base refinement - set result to stack
                paintSpatialMemory(refinements, result_ref_idx, .{ .stack = .{ .meta = ctx.meta, .root_gid = null } });
                return;
            },
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                // Interned global - set .interned
                interned_init(refinements, result_ref_idx);
                return;
            },
            .fnptr => {
                // Constant - set .interned
                interned_init(refinements, result_ref_idx);
                return;
            },
        };
        const base_refinement = refinements.at(base_ref).*;

        // For slices: base is pointer → region
        if (base_refinement != .pointer) {
            paintSpatialMemory(refinements, result_ref_idx, .{ .stack = .{ .meta = ctx.meta, .root_gid = null } });
            return;
        }
        const region_idx = base_refinement.pointer.to;
        const region_ref = refinements.at(region_idx);
        if (region_ref.* != .region) {
            paintSpatialMemory(refinements, result_ref_idx, .{ .stack = .{ .meta = ctx.meta, .root_gid = null } });
            return;
        }

        const ms = region_ref.region.analyte.memory_safety orelse {
            // Region has no memory_safety - set .interned (likely static data)
            interned_init(refinements, result_ref_idx);
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
            .error_stub => {
                result_ref.pointer.analyte.memory_safety = .{ .stack = .{
                    .meta = ctx.meta,
                    .root_gid = region_idx,
                } };
            },
            .placeholder => std.debug.panic("placeholder in slice_elem_ptr - region not initialized", .{}),
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

        const orig_analyte = switch (orig_ref.*) {
            .void, .noreturn, .unimplemented => return,
            else => getAnalytePtr(orig_ref),
        };

        // Determine if this is a loop merge (where index 0 is the null case)
        const is_loop_merge = comptime merge_tag == .loop;

        // Handle allocation freed state merging
        // If original has an unfreed allocation, check if branches freed it
        if (orig_analyte.memory_safety) |*orig_ms| {
            switch (orig_ms.*) {
                .placeholder => std.debug.panic("placeholder in merge - entity not initialized", .{}),
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
                            const branch_analyte = switch (branch_ref.*) {
                                .void, .noreturn, .unimplemented => continue,
                                else => getAnalytePtr(branch_ref),
                            };
                            // Branch may have different memory_safety state (e.g., null branch has null)
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
                .stack, .interned => {
                    // For return slot merges: the original may have placeholder .stack/.interned
                    // from splatInit, while branches have the actual returned value's
                    // memory_safety. Take the branch's value if it exists.
                    for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                        const branch = branch_opt orelse continue;
                        const branch_gid = branch_gid_opt orelse continue;
                        const branch_ref = branch.refinements.at(branch_gid);
                        const branch_analyte = switch (branch_ref.*) {
                            .void, .noreturn, .unimplemented => continue,
                            else => getAnalytePtr(branch_ref),
                        };
                        if (branch_analyte.memory_safety) |ms| {
                            orig_analyte.memory_safety = ms;
                            break;
                        }
                    }
                },
                .error_stub => {}, // unwrap_errunion_payload rejects this at the tag layer
            }
        } else {
            // Original has no memory_safety - copy from first branch that has it
            for (branches, branch_gids) |branch_opt, branch_gid_opt| {
                // Null branch = unreachable path
                const branch = branch_opt orelse continue;
                // Entity may not exist in all branches during recursive merge traversal
                const branch_gid = branch_gid_opt orelse continue;
                const branch_ref = branch.refinements.at(branch_gid);
                const branch_analyte = switch (branch_ref.*) {
                    .void, .noreturn, .unimplemented => continue,
                    else => getAnalytePtr(branch_ref),
                };
                if (branch_analyte.memory_safety) |ms| {
                    orig_analyte.memory_safety = ms;
                    break;
                }
            }
        }
    }

    /// Handle orphaned entities detected during branch merge or function end.
    /// An orphaned entity was created but is no longer reachable.
    /// If the orphaned entity is a pointer to an unfreed allocation, report a leak.
    ///
    /// For branch_merge: entities that "fall off" during if/else/switch/loop merge
    /// For function_end: entities not reachable from roots at function exit
    pub fn orphaned(
        ctx: *Context,
        refinements: *Refinements,
        orphan_refinements: *Refinements,
        gid: Gid,
        copied_from_branch: ?*const std.AutoHashMap(Gid, void),
        orphan_ctx: core.OrphanContext,
    ) !void {
        const ref = orphan_refinements.at(gid);

        // Only check pointers - allocation state is on the pointee
        if (ref.* != .pointer) return;

        // Get the pointee entity via ptr.to
        const pointee_idx = ref.pointer.to;

        // For branch_merge: If the pointee was copied to the parent table, the allocation is not truly orphaned.
        // This happens when we store an allocation into a struct field - the field's .to gets
        // updated to point to the allocation, and during merge we copy the allocation (pointee)
        // but not the intermediate pointer. The pointer is "orphaned" but its allocation lives on.
        if (copied_from_branch) |cfb| {
            if (cfb.contains(pointee_idx)) return;
        }

        const pointee = orphan_refinements.at(pointee_idx);
        const pointee_analyte = getAnalytePtr(pointee);

        // Check pointee's memory_safety for allocation state
        const ms = pointee_analyte.memory_safety orelse return;
        if (ms != .allocated) return;

        const allocation = ms.allocated;

        // If allocation is already freed, not a leak
        if (allocation.freed != null) return;

        // Check if allocator was deinited (for arena allocators)
        // The allocator GID is stable, so we can check directly.
        const alloc_ref = orphan_refinements.at(allocation.allocator_gid).allocator;
        if (alloc_ref.deinit != null) return;

        // For branch_merge: Check if an allocation using the same allocator exists in entities that are
        // still reachable after merge. This includes:
        // 1. Entities in copied_from_branch (explicitly copied to parent)
        // 2. Entities with GID < base_len (existed before branch, still reachable)
        // 3. Entities reachable through pointers that were updated during branch
        //
        // When we copy entities during branch operations, the copies inherit .allocated memory_safety.
        // If ANY entity using the same allocator is still reachable, it's not a leak.
        if (orphan_ctx == .branch_merge) {
            const base_len = orphan_ctx.branch_merge.base_len;
            const allocator_gid = allocation.allocator_gid;

            // Check if this allocator is represented in any entity that was copied to parent
            if (copied_from_branch) |cfb| {
                var it = cfb.keyIterator();
                while (it.next()) |copied_gid| {
                    if (hasMatchingAllocation(orphan_refinements, copied_gid.*, allocator_gid)) {
                        return;
                    }
                }
            }

            // Check pre-branch entities in the PARENT refinements.
            // When a branch stores an allocation into an argument struct, the parent's
            // refinements get updated. So we must check the parent, not the branch.
            for (0..base_len) |pre_gid| {
                if (hasMatchingAllocation(refinements, @intCast(pre_gid), allocator_gid)) {
                    return;
                }
            }

            // Also check in orphan_refinements (branch) - allocations stored through
            // argument pointers update the branch's copy of the argument entity
            for (0..base_len) |pre_gid| {
                if (hasMatchingAllocation(orphan_refinements, @intCast(pre_gid), allocator_gid)) {
                    return;
                }
            }
        }

        // Report leak with context-appropriate message
        const is_arena = alloc_ref.arena_gid != null;
        return reportMemoryLeakWithContext(ctx, allocation, orphan_ctx, is_arena);
    }

    /// Report a memory leak with context about how/where it was detected.
    fn reportMemoryLeakWithContext(ctx: *Context, allocation: Allocated, orphan_ctx: core.OrphanContext, is_arena: bool) anyerror {
        const leak_type = if (is_arena) "arena leak" else "memory leak";

        switch (orphan_ctx) {
            .branch_merge => |bm| {
                const branch_name = switch (bm.branch_type) {
                    .cond_br => "if/else",
                    .switch_br => "switch",
                    .loop => "loop",
                    .loop_switch_br => "labeled switch",
                };
                try bm.meta.print(ctx.writer, "{s} detected at {s} merge in ", .{ leak_type, branch_name });
            },
            .function_end => {
                // Use same format as old reportMemoryLeak for compatibility
                try ctx.meta.print(ctx.writer, "{s} in ", .{leak_type});
            },
        }

        var buf: [256]u8 = undefined;
        const name_prefix = formatNamePrefix(allocation.name_at_alloc, &buf);
        if (name_prefix.len > 0) {
            try allocation.meta.print(ctx.writer, "{s}allocated in ", .{name_prefix});
        } else {
            try allocation.meta.print(ctx.writer, "allocated in ", .{});
        }

        return if (is_arena) error.ArenaLeak else error.MemoryLeak;
    }

    /// Fresh return slots keep null memory_safety until real values are written into them.
    pub fn retval_init(refinements: *Refinements, gid: Gid, ctx: *Context) void {
        // Return slots are stack-allocated local values.
        // Use paintReturnSlotMemory to traverse the full structure including .to fields,
        // since typeToRefinement creates nested entities that all need initialization.
        const stack_ms: MemorySafety = .{ .stack = .{ .meta = ctx.meta, .root_gid = null } };
        paintReturnSlotMemory(refinements, gid, stack_ms);
    }

    /// Initialize memory_safety for defined scalar values produced by operations.
    /// These are local stack values (like SIMD reduce results) that need memory_safety
    /// set to .stack so they can be wrapped in optionals, error unions, etc.
    /// Unlike paintSpatialMemory, this ALSO traverses pointer.to since we're setting up
    /// the entire return slot structure at once (pointee structures are part of the slot).
    pub fn retval_init_defined(refinements: *Refinements, gid: Gid) void {
        // Set stack memory_safety for scalar values
        // Note: No specific meta location since this is a computed result
        const stack_ms: MemorySafety = .{ .stack = .{ .meta = .{
            .function = "",
            .file = "<computed>",
            .line = 0,
            .column = 0,
        }, .root_gid = null } };
        paintReturnSlotMemory(refinements, gid, stack_ms);
    }

    /// Like paintSpatialMemory but also traverses pointer.to fields.
    /// Used for return slot initialization where the entire nested type structure
    /// (including pointee placeholders) was created together and needs memory_safety.
    fn paintReturnSlotMemory(refinements: *Refinements, gid: Gid, ms: MemorySafety) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.memory_safety = ms;
            },
            .pointer => |p| {
                ref.pointer.analyte.memory_safety = ms;
                // Unlike paintSpatialMemory, traverse .to for return slot structures
                paintReturnSlotMemory(refinements, p.to, ms);
            },
            .optional => |o| {
                ref.optional.analyte.memory_safety = ms;
                paintReturnSlotMemory(refinements, o.to, ms);
            },
            .errorunion => |e| {
                ref.errorunion.analyte.memory_safety = ms;
                paintReturnSlotMemory(refinements, e.to, ms);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = ms;
                for (s.fields) |field_gid| {
                    paintReturnSlotMemory(refinements, field_gid, ms);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = ms;
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        paintReturnSlotMemory(refinements, field_gid, ms);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.memory_safety = ms;
            },
            .fnptr => {
                ref.fnptr.analyte.memory_safety = ms;
            },
            .region => |r| {
                ref.region.analyte.memory_safety = ms;
                paintReturnSlotMemory(refinements, r.to, ms);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = ms;
                if (r.to != 0) {
                    paintReturnSlotMemory(refinements, r.to, ms);
                }
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Initialize memory_safety for comptime/interned values.
    /// These are compile-time constants and should have .interned memory_safety.
    /// This is called when creating entities for interned Src values that are
    /// not tracked globals (e.g., &global_value pointer constants).
    /// Uses paintReturnSlotMemoryForce to traverse pointer.to, since interned pointers
    /// point to interned data (both the pointer value and pointee are comptime).
    /// Force=true because this needs to override .stack set by splatInit which runs first.
    pub fn interned_init(refinements: *Refinements, gid: Gid) void {
        paintReturnSlotMemoryForce(refinements, gid, .{ .interned = comptime_interned_meta });
    }

    /// Initialize memory_safety on a refinement created from a `.undefined` type wrapper.
    /// Undefined pointers are represented as placeholder pointer values until a real
    /// runtime value is stored into them.
    pub fn init_undefined(ref: *Refinements.Refinement, meta: Meta) void {
        _ = meta;
        switch (ref.*) {
            .pointer => |*p| p.analyte.memory_safety = .{ .placeholder = {} },
            else => {},
        }
    }

    /// Like paintReturnSlotMemory but always overwrites existing values.
    /// Used by interned_init which needs to override .stack set by splatInit.
    fn paintReturnSlotMemoryForce(refinements: *Refinements, gid: Gid, ms: MemorySafety) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.memory_safety = ms;
            },
            .pointer => |p| {
                ref.pointer.analyte.memory_safety = ms;
                paintReturnSlotMemoryForce(refinements, p.to, ms);
            },
            .optional => |o| {
                ref.optional.analyte.memory_safety = ms;
                paintReturnSlotMemoryForce(refinements, o.to, ms);
            },
            .errorunion => |e| {
                ref.errorunion.analyte.memory_safety = ms;
                paintReturnSlotMemoryForce(refinements, e.to, ms);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = ms;
                for (s.fields) |field_gid| {
                    paintReturnSlotMemoryForce(refinements, field_gid, ms);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = ms;
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        paintReturnSlotMemoryForce(refinements, field_gid, ms);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.memory_safety = ms;
            },
            .fnptr => {
                ref.fnptr.analyte.memory_safety = ms;
            },
            .region => |r| {
                ref.region.analyte.memory_safety = ms;
                paintReturnSlotMemoryForce(refinements, r.to, ms);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = ms;
                if (r.to != 0) {
                    paintReturnSlotMemoryForce(refinements, r.to, ms);
                }
            },
            .void, .noreturn, .unimplemented => {},
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
    // Simple operation handlers - set memory_safety to .stack for computed values
    // =========================================================================

    /// Helper to set memory_safety = .stack on a scalar result (computed values live on stack)
    fn setResultStack(state: State, index: usize) void {
        const ref_idx = state.results[index].refinement orelse return;
        state.refinements.at(ref_idx).scalar.analyte.memory_safety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };
    }

    /// Helper to set memory_safety = .stack on a struct and its fields (computed values live on stack)
    fn setStructStack(state: State, index: usize) void {
        const ref_idx = state.results[index].refinement orelse return;
        const stack_ms: MemorySafety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };
        paintSpatialMemory(state.refinements, ref_idx, stack_ms);
    }

    // Simple arithmetic/comparison operations - produce computed scalars
    pub fn bit_and(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn cmp_eq(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn cmp_gt(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn cmp_gte(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn cmp_lt(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn cmp_lte(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn ctz(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn sub(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn add(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn slice_len(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn get_union_tag(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn is_non_null(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn is_null(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn is_non_null_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn is_null_ptr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn is_non_err(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    /// optional_payload_ptr creates a pointer to the payload.
    /// Propagate memory_safety from source pointer (with root_gid for derived pointer tracking).
    pub fn optional_payload_ptr(state: State, index: usize, params: tag.OptionalPayloadPtr) !void {
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &state.refinements.at(ptr_idx).pointer;

        // Get source pointer's memory_safety and propagate it
        const src_gid: ?Gid = switch (params.src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => |interned| state.refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        if (src_gid) |src| {
            const src_ref = state.refinements.at(src);
            if (src_ref.* == .pointer) {
                if (src_ref.pointer.analyte.memory_safety) |src_ms| {
                    ptr.analyte.memory_safety = switch (src_ms) {
                        .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = src } },
                        .allocated => |a| .{ .allocated = .{
                            .meta = a.meta,
                            .allocator_gid = a.allocator_gid,
                            .type_id = a.type_id,
                            .freed = a.freed,
                            .root_gid = src,
                        } },
                        .interned => |g| .{ .interned = g },
                        .error_stub => @panic("optional_payload_ptr: error_stub source is not a valid pointer source"),
                        .placeholder => @panic("optional_payload_ptr: placeholder source is not a valid pointer source"),
                    };
                }
            }
        }
    }

    /// unwrap_errunion_payload_ptr creates a pointer to the error union payload.
    /// Propagate memory_safety from source pointer (with root_gid for derived pointer tracking).
    pub fn unwrap_errunion_payload_ptr(state: State, index: usize, params: tag.UnwrapErrunionPayloadPtr) !void {
        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &state.refinements.at(ptr_idx).pointer;

        // Get source pointer's memory_safety and propagate it
        const src_gid: ?Gid = switch (params.src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => |interned| state.refinements.getGlobal(interned.ip_idx),
            .fnptr => null,
        };
        if (src_gid) |src| {
            const src_ref = state.refinements.at(src);
            if (src_ref.* == .pointer) {
                if (src_ref.pointer.analyte.memory_safety) |src_ms| {
                    ptr.analyte.memory_safety = switch (src_ms) {
                        .stack => |s| .{ .stack = .{ .meta = s.meta, .root_gid = src } },
                        .allocated => |a| .{ .allocated = .{
                            .meta = a.meta,
                            .allocator_gid = a.allocator_gid,
                            .type_id = a.type_id,
                            .freed = a.freed,
                            .root_gid = src,
                        } },
                        .interned => |g| .{ .interned = g },
                        .error_stub => @panic("unwrap_errunion_payload_ptr: error_stub source is not a valid pointer source"),
                        .placeholder => @panic("unwrap_errunion_payload_ptr: placeholder source is not a valid pointer source"),
                    };
                }
            }
        }
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
            paintSpatialMemory(refinements, gid, .{ .stack = .{ .meta = ctx.meta, .root_gid = null } });
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
        setStructStack(state, index);
    }

    pub fn sub_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        setStructStack(state, index);
    }

    pub fn mul_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        setStructStack(state, index);
    }

    /// array_to_slice converts array/many-pointer to slice.
    /// If the source has no memory_safety, the new slice pointer value lives on the
    /// stack, but the underlying region keeps its existing spatial memory.
    pub fn array_to_slice(state: State, index: usize, params: anytype) !void {
        _ = params;
        const refinements = state.refinements;

        const ptr_idx = state.results[index].refinement orelse return;
        const ptr = &refinements.at(ptr_idx).pointer;

        // If pointer already has memory_safety, nothing to do (shared from source)
        if (ptr.analyte.memory_safety != null) return;

        // Source had no memory_safety - paint only the top-level pointer.
        // paintSpatialMemory stops at pointer indirection, so the region keeps
        // whatever spatial memory it already has.
        paintSpatialMemory(refinements, ptr_idx, .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } });
    }

    // Simple operations that produce scalar values
    pub fn ret_addr(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn intcast(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
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
                        // No source element - field uses default value.
                        // Pointer fields default to comptime values (empty slice, null, etc.) -> .interned
                        // Non-pointer fields are computed values -> .stack
                        const field_ref = state.refinements.at(field_gid);
                        if (field_ref.* == .pointer) {
                            paintSpatialMemory(state.refinements, field_gid, .{ .interned = state.ctx.meta });
                        } else {
                            paintSpatialMemory(state.refinements, field_gid, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
                        }
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
                    paintSpatialMemory(state.refinements, r.to, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
                }
            },
            else => {
                // Fallback: initialize to stack (computed value)
                paintSpatialMemory(state.refinements, result_gid, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
            },
        }
    }

    /// Copy memory_safety state from a source to a destination refinement.
    /// For interned/fnptr sources, destination is set to .interned (they are comptime values).
    fn copyMemorySafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values - set .interned
            .fnptr => null, // Function pointers - set .interned
        };

        // For interned/fnptr sources, set .interned on destination
        if (src_gid == null) {
            interned_init(state.refinements, dst_gid);
            return;
        }

        // Copy analyte state recursively from source to destination
        copyMemorySafetyStateRecursive(state.refinements, dst_gid, src_gid.?);
    }

    /// Recursively copy memory_safety state from source GID to destination GID.
    /// If source doesn't have memory_safety, destination is left as null (no tracking).
    fn copyMemorySafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        // Copy memory_safety analyte based on type
        switch (dst_ref.*) {
            .scalar => |*s| {
                s.analyte.memory_safety = switch (src_ref.*) {
                    .scalar => |ss| ss.analyte.memory_safety,
                    else => null, // Type mismatch - leave untracked
                };
            },
            .pointer => |*p| {
                p.analyte.memory_safety = switch (src_ref.*) {
                    .pointer => |sp| sp.analyte.memory_safety,
                    else => null, // Type mismatch - leave untracked
                };
                // Also copy pointee state
                if (src_ref.* == .pointer) {
                    copyMemorySafetyStateRecursive(refinements, p.to, src_ref.pointer.to);
                }
                // If type mismatch, pointee keeps its default null memory_safety
            },
            .optional => |*o| {
                o.analyte.memory_safety = switch (src_ref.*) {
                    .optional => |so| so.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .optional) {
                    copyMemorySafetyStateRecursive(refinements, o.to, src_ref.optional.to);
                }
            },
            .errorunion => |*e| {
                e.analyte.memory_safety = switch (src_ref.*) {
                    .errorunion => |se| se.analyte.memory_safety,
                    else => null,
                };
                if (src_ref.* == .errorunion) {
                    copyMemorySafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
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
                        }
                        // Extra fields keep default null memory_safety
                    }
                }
                // If type mismatch, fields keep default null memory_safety
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
                            }
                            // Else source field null - dest keeps default null memory_safety
                        }
                        // Extra fields keep default null memory_safety
                    }
                }
                // If type mismatch, fields keep default null memory_safety
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
                }
                // If type mismatch, element keeps default null memory_safety
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
        const ref = state.refinements.at(ref_idx);
        if (ref.* != .errorunion) return;
        ref.errorunion.analyte.memory_safety = .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } };
        markPayloadErrorStub(state.refinements, ref.errorunion.to);
    }

    pub fn unwrap_errunion_err(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    pub fn slice(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
    }

    pub fn dbg_inline_block(state: State, index: usize, params: anytype) !void {
        _ = params;
        setResultStack(state, index);
    }

    // =========================================================================
    // typeToRefinement initialization handlers
    // These set memory_safety on refinements created by typeToRefinement
    // =========================================================================

    /// Paint spatial memory over a newly materialized value tree.
    ///
    /// This traverses non-pointer structure and stops at pointer indirection.
    /// Callers must only use this on newly materialized result structure, never
    /// on shared preexisting nodes.
    pub fn paintSpatialMemory(refinements: *Refinements, gid: Gid, ms: MemorySafety) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.memory_safety = ms;
            },
            .pointer => {
                ref.pointer.analyte.memory_safety = ms;
            },
            .optional => |o| {
                ref.optional.analyte.memory_safety = ms;
                paintSpatialMemory(refinements, o.to, ms);
            },
            .errorunion => |e| {
                ref.errorunion.analyte.memory_safety = ms;
                paintSpatialMemory(refinements, e.to, ms);
            },
            .@"struct" => |s| {
                ref.@"struct".analyte.memory_safety = ms;
                for (s.fields) |field_gid| {
                    paintSpatialMemory(refinements, field_gid, ms);
                }
            },
            .@"union" => |u| {
                ref.@"union".analyte.memory_safety = ms;
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        paintSpatialMemory(refinements, field_gid, ms);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.memory_safety = ms;
            },
            .fnptr => {
                ref.fnptr.analyte.memory_safety = ms;
            },
            .region => |r| {
                ref.region.analyte.memory_safety = ms;
                paintSpatialMemory(refinements, r.to, ms);
            },
            .recursive => |r| {
                ref.recursive.analyte.memory_safety = ms;
                if (r.to != 0) {
                    paintSpatialMemory(refinements, r.to, ms);
                }
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Copy memory_safety from source refinement to destination refinement recursively.
    /// Used when storing a struct value to copy the source's field memory_safety to the destination.
    /// Unlike paintSpatialMemory which sets a uniform value, this copies from corresponding source fields.
    pub fn copyMemorySafetyRecursive(refinements: *Refinements, dest_gid: Gid, src_gid: Gid) void {
        const dest_ref = refinements.at(dest_gid);
        const src_ref = refinements.at(src_gid);

        switch (dest_ref.*) {
            .scalar => {
                if (src_ref.* == .scalar) {
                    if (src_ref.scalar.analyte.memory_safety) |ms| {
                        dest_ref.scalar.analyte.memory_safety = ms;
                    }
                }
            },
            .pointer => {
                if (src_ref.* == .pointer) {
                    if (src_ref.pointer.analyte.memory_safety) |ms| {
                        dest_ref.pointer.analyte.memory_safety = ms;
                    }
                    // Also copy pointee state (added for struct stores)
                    copyMemorySafetyRecursive(refinements, dest_ref.pointer.to, src_ref.pointer.to);
                }
            },
            .optional => |dest_o| {
                if (src_ref.* == .optional) {
                    const src_o = src_ref.optional;
                    if (src_o.analyte.memory_safety) |ms| {
                        dest_ref.optional.analyte.memory_safety = ms;
                    }
                    copyMemorySafetyRecursive(refinements, dest_o.to, src_o.to);
                }
            },
            .errorunion => |dest_e| {
                if (src_ref.* == .errorunion) {
                    const src_e = src_ref.errorunion;
                    if (src_e.analyte.memory_safety) |ms| {
                        dest_ref.errorunion.analyte.memory_safety = ms;
                    }
                    copyMemorySafetyRecursive(refinements, dest_e.to, src_e.to);
                }
            },
            .@"struct" => |dest_s| {
                if (src_ref.* == .@"struct") {
                    const src_s = src_ref.@"struct";
                    if (src_s.analyte.memory_safety) |ms| {
                        dest_ref.@"struct".analyte.memory_safety = ms;
                    }
                    for (dest_s.fields, src_s.fields) |dest_field_gid, src_field_gid| {
                        copyMemorySafetyRecursive(refinements, dest_field_gid, src_field_gid);
                    }
                }
            },
            .@"union" => |dest_u| {
                if (src_ref.* == .@"union") {
                    const src_u = src_ref.@"union";
                    if (src_u.analyte.memory_safety) |ms| {
                        dest_ref.@"union".analyte.memory_safety = ms;
                    }
                    for (dest_u.fields, src_u.fields) |maybe_dest_gid, maybe_src_gid| {
                        if (maybe_dest_gid) |dest_field_gid| {
                            if (maybe_src_gid) |src_field_gid| {
                                copyMemorySafetyRecursive(refinements, dest_field_gid, src_field_gid);
                            }
                        }
                    }
                }
            },
            .allocator => {
                if (src_ref.* == .allocator) {
                    if (src_ref.allocator.analyte.memory_safety) |ms| {
                        dest_ref.allocator.analyte.memory_safety = ms;
                    }
                }
            },
            .fnptr => {
                if (src_ref.* == .fnptr) {
                    if (src_ref.fnptr.analyte.memory_safety) |ms| {
                        dest_ref.fnptr.analyte.memory_safety = ms;
                    }
                }
            },
            .region => |dest_r| {
                if (src_ref.* == .region) {
                    const src_r = src_ref.region;
                    if (src_r.analyte.memory_safety) |ms| {
                        dest_ref.region.analyte.memory_safety = ms;
                    }
                    copyMemorySafetyRecursive(refinements, dest_r.to, src_r.to);
                }
            },
            .recursive => |dest_r| {
                if (src_ref.* == .recursive) {
                    const src_r = src_ref.recursive;
                    if (src_r.analyte.memory_safety) |ms| {
                        dest_ref.recursive.analyte.memory_safety = ms;
                    }
                    if (dest_r.to != 0 and src_r.to != 0) {
                        copyMemorySafetyRecursive(refinements, dest_r.to, src_r.to);
                    }
                }
            },
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Block creates result via typeToRefinement - initialize memory_safety
    pub fn block(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
    }

    /// MkAllocator creates an allocator refinement - initialize memory_safety
    pub fn mkallocator(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        const ref = state.refinements.at(ref_idx);
        ref.allocator.analyte.memory_safety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };
    }

    /// StructFieldVal may create refinement via typeToRefinement for interned/global
    /// structs or for inactive union fields - initialize memory_safety
    pub fn struct_field_val(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        // Only init if memory_safety is null (not already set by container)
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
    }

    /// UnionInit creates union entity and may create field via typeToRefinement
    pub fn union_init(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
    }

    /// Bitcast may create optional wrapper - initialize memory_safety
    pub fn bitcast(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
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
        paintSpatialMemory(state.refinements, field_gid, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
    }

    /// WrapErrunionPayload may create payload via typeToRefinement
    pub fn wrap_errunion_payload(state: State, index: usize, params: anytype) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);
        if (result_ref.* != .errorunion) return;

        const payload_gid = result_ref.errorunion.to;
        const payload_ref = state.refinements.at(payload_gid);
        const payload_ms = switch (payload_ref.*) {
            .scalar => |s| s.analyte.memory_safety,
            .pointer => |p| p.analyte.memory_safety,
            .optional => |o| o.analyte.memory_safety,
            .errorunion => |e| e.analyte.memory_safety,
            .@"struct" => |s| s.analyte.memory_safety,
            .@"union" => |u| u.analyte.memory_safety,
            .allocator => |a| a.analyte.memory_safety,
            .fnptr => |f| f.analyte.memory_safety,
            .region => |r| r.analyte.memory_safety,
            .recursive => |r| r.analyte.memory_safety,
            .void, .noreturn, .unimplemented => null,
        } orelse std.debug.panic("wrap_errunion_payload: payload has no memory_safety", .{});

        // The errorunion WRAPPER is a new computed value on the stack (or interned if from comptime).
        // The PAYLOAD can be anything - stack, allocated, or interned - it retains its own memory_safety.
        // For example, toOwnedSlice returns errorunion!([]u8) where the payload is an allocated slice.
        _ = payload_ms; // Payload's memory_safety is preserved, not overwritten
        const paint: MemorySafety = switch (params.src) {
            .interned, .fnptr => .{ .interned = comptime_interned_meta },
            .inst => .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } },
        };

        result_ref.errorunion.analyte.memory_safety = paint;
    }

    /// WrapOptional creates an optional refinement - initialize memory_safety
    pub fn wrap_optional(state: State, index: usize, params: anytype) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);
        if (result_ref.* != .optional) return;

        const payload_gid = result_ref.optional.to;
        const payload_ref = state.refinements.at(payload_gid);
        const payload_ms = switch (payload_ref.*) {
            .scalar => |s| s.analyte.memory_safety,
            .pointer => |p| p.analyte.memory_safety,
            .optional => |o| o.analyte.memory_safety,
            .errorunion => |e| e.analyte.memory_safety,
            .@"struct" => |s| s.analyte.memory_safety,
            .@"union" => |u| u.analyte.memory_safety,
            .allocator => |a| a.analyte.memory_safety,
            .fnptr => |f| f.analyte.memory_safety,
            .region => |r| r.analyte.memory_safety,
            .recursive => |r| r.analyte.memory_safety,
            .void, .noreturn, .unimplemented => null,
        } orelse std.debug.panic("wrap_optional: payload has no memory_safety", .{});

        // The optional WRAPPER is a new computed value on the stack (or interned if from comptime).
        // The PAYLOAD can be anything - stack, allocated, or interned - it retains its own memory_safety.
        // For example, an optional containing a pointer to allocated memory.
        _ = payload_ms; // Payload's memory_safety is preserved, not overwritten
        const paint: MemorySafety = switch (params.src) {
            .interned, .fnptr => .{ .interned = comptime_interned_meta },
            .inst => .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } },
        };

        result_ref.optional.analyte.memory_safety = paint;
    }

    /// RetPtr creates a return value entity via typeToRefinement for struct/union returns
    pub fn ret_ptr(state: State, index: usize, params: anytype) !void {
        // RetPtr creates a return value entity via typeToRefinement for struct/union returns.
        // That entity starts with .placeholder memory_safety - set to .stack since it's a
        // local stack value used to build the return value.
        _ = params;
        const ptr_ref_idx = state.results[index].refinement orelse return;
        const ptr_ref = state.refinements.at(ptr_ref_idx);
        if (ptr_ref.* != .pointer) return;
        const stack_ms: MemorySafety = .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } };
        // Set stack memory_safety on the pointer itself
        ptr_ref.pointer.analyte.memory_safety = stack_ms;
        // Set stack memory_safety on the pointee (the return value entity)
        paintSpatialMemory(state.refinements, ptr_ref.pointer.to, stack_ms);
    }

    /// ErrunionPayloadPtrSet creates a new pointer to the payload - initialize memory_safety
    pub fn errunion_payload_ptr_set(state: State, index: usize, params: anytype) !void {
        _ = params;
        const ref_idx = state.results[index].refinement orelse return;
        paintSpatialMemory(state.refinements, ref_idx, .{ .stack = .{ .meta = state.ctx.meta, .root_gid = null } });
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
        paintSpatialMemory(refinements, pointee_gid, .{ .interned = meta });
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
            setResultStack(state, index);
            return true;
        }

        // realloc: frees old slice, allocates new slice
        if (gates.isAllocatorRealloc(fqn)) {
            try handleAllocRealloc(state, index, args, false);
            return true;
        }

        // remap: attempts to resize in place, returns null on failure - doesn't free old memory
        // For remap, we don't report errors for interned memory since it just returns null
        if (gates.isAllocatorRemap(fqn)) {
            try handleAllocRealloc(state, index, args, true);
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
            setResultStack(state, index);
            return true;
        }

        // MkAllocator: call returns std.mem.Allocator
        // The allocator refinement is already created by typeToRefinement.
        // We just need to link it to the arena if this is ArenaAllocator.allocator().
        if (return_type == .allocator) {
            try handleMkAllocator(state, index, args, fqn);
            return true;
        }

        // bufPrint/bufPrintZ: returns slice into first arg (buffer)
        // Propagate buffer's memory_safety to returned slice
        if (gates.isFmtBufPrint(fqn) or gates.isFmtBufPrintZ(fqn)) {
            try handleBufPrint(state, index, args);
            return true;
        }

        // Formatter functions - check tuple args for use-after-free
        if (gates.isFormatter(fqn)) {
            try handleFormatter(state, args);
            return true; // Intercept - stub is generated
        }

        return false;
    }

    /// Handle bufPrint/bufPrintZ - returns slice into buffer (first arg)
    /// Propagate buffer's memory_safety to returned slice
    fn handleBufPrint(state: State, index: usize, args: []const tag.Src) !void {
        const results = state.results;
        const refinements = state.refinements;

        // First check all args for use-after-free
        try handleFormatter(state, args);

        // Get the buffer's memory_safety from args[0]
        // Buffer is a slice (pointer -> region)
        if (args.len == 0) return;
        const buf_gid: Gid = switch (args[0]) {
            .inst => |inst| results[inst].refinement orelse return,
            .interned, .fnptr => return,
        };
        const buf_ref = refinements.at(buf_gid);
        if (buf_ref.* != .pointer) return;
        const buf_ms = buf_ref.pointer.analyte.memory_safety orelse return;

        // Get the return value refinement (errorunion -> pointer)
        const result_gid = results[index].refinement orelse return;
        const result_ref = refinements.at(result_gid);
        if (result_ref.* != .errorunion) return;

        const payload_gid = result_ref.errorunion.to;
        const payload_ref = refinements.at(payload_gid);
        if (payload_ref.* != .pointer) return;

        // Propagate buffer's memory_safety to the returned slice
        switch (buf_ms) {
            .stack => |stack| {
                refinements.at(payload_gid).pointer.analyte.memory_safety = .{
                    .stack = .{
                        .meta = stack.meta,
                        .name = stack.name,
                        .root_gid = stack.root_gid,
                    },
                };
            },
            .allocated => |alloc_ms| {
                refinements.at(payload_gid).pointer.analyte.memory_safety = .{
                    .allocated = .{
                        .meta = alloc_ms.meta,
                        .allocator_gid = alloc_ms.allocator_gid,
                        .type_id = alloc_ms.type_id,
                        .root_gid = buf_gid, // Derived from buffer
                        .freed = alloc_ms.freed,
                        .name_at_alloc = alloc_ms.name_at_alloc,
                    },
                };
            },
            .interned => |meta| {
                paintSpatialMemory(refinements, payload_gid, .{ .interned = meta });
            },
            .error_stub => {},
            .placeholder => std.debug.panic("placeholder in memset - buffer not initialized", .{}),
        }

        // Also propagate to the region if it exists
        if (payload_ref.pointer.to != 0) {
            const region_gid = payload_ref.pointer.to;
            const region_ref = refinements.at(region_gid);
            if (region_ref.* == .region) {
                switch (buf_ms) {
                    .stack => |stack| {
                        region_ref.region.analyte.memory_safety = .{ .stack = stack };
                    },
                    .allocated => |alloc_ms| {
                        region_ref.region.analyte.memory_safety = .{ .allocated = alloc_ms };
                    },
                    .interned => |meta| {
                        paintSpatialMemory(refinements, region_gid, .{ .interned = meta });
                    },
                    .error_stub => {},
                    .placeholder => {}, // Already panicked above
                }
            }
        }
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
                    .stack, .interned, .error_stub, .placeholder => {},
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

        // The errorunion wrapper itself is a fresh computed value on the stack.
        eu.analyte.memory_safety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };

        // Set allocation state recursively on the pointee
        // Note: We only set memory_safety on the POINTEE, not the pointer itself.
        // The pointer is a return value (register/stack), the pointee is on the heap.
        paintSpatialMemory(state.refinements, pointee_ref, .{ .allocated = .{
            .meta = alloc_base.meta,
            .allocator_gid = alloc_base.allocator_gid,
            .type_id = alloc_base.type_id,
            .root_gid = null,
        } });
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
            .error_stub => return,
            .placeholder => std.debug.panic("placeholder in free - entity not initialized", .{}),
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
        // Skip check if both type_ids are 0 (unknown) - can't verify without type info.
        // This happens when allocators are passed through function arguments without
        // going through MkAllocator, causing each load to create a new GID.
        if (destroy_allocator_gid) |dag| {
            if (a.allocator_gid != dag) {
                // GIDs differ - check if we can verify via type_id
                // If both type_ids are 0, we can't determine mismatch (skip check)
                // If both type_ids are non-zero and match, it's the same allocator type
                if (a.type_id != 0 or destroy_type_id != 0) {
                    if (a.type_id != destroy_type_id) {
                        return reportMismatchedAllocator(ctx, a, destroy_type_id);
                    }
                }
            }
        }

        // destroy expects single item (create), not slice (alloc)
        // Slice allocations have metadata ON the region; single-item have metadata on scalar/struct
        if (pointee.* == .region) {
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

        // The errorunion wrapper itself is a fresh computed value on the stack.
        eu.analyte.memory_safety = .{ .stack = .{
            .meta = state.ctx.meta,
            .root_gid = null,
        } };

        // Note: We only set memory_safety on the REGION and ELEMENT, not the pointer itself.
        // The pointer is a return value (register/stack), the region/element are on the heap.

        // Set memory_safety on region (alloc allocations have metadata ON the region)
        region.analyte.memory_safety = .{
            .allocated = .{
                .meta = alloc_base.meta,
                .allocator_gid = alloc_base.allocator_gid,
                .type_id = type_id,
                .root_gid = null,
            },
        };

        // Set allocation state recursively on the element
        paintSpatialMemory(state.refinements, element_ref, .{ .allocated = .{
            .meta = alloc_base.meta,
            .allocator_gid = alloc_base.allocator_gid,
            .type_id = alloc_base.type_id,
            .root_gid = null,
        } });
    }

    const AllocatorIdentity = struct {
        gid: Gid,
        type_id: u32,
    };

    fn resolveAllocatorIdentity(
        results: []Inst,
        refinements: *Refinements,
        src: tag.Src,
        comptime panic_name: []const u8,
    ) ?AllocatorIdentity {
        return switch (src) {
            .inst => |inst| blk: {
                const arg_gid = results[inst].refinement orelse break :blk null;
                const ref = refinements.at(arg_gid);
                if (ref.* != .allocator) break :blk null;
                break :blk .{ .gid = arg_gid, .type_id = ref.allocator.type_id };
            },
            .interned => |interned| blk: {
                if (interned.ty != .allocator) break :blk null;
                const ptr_gid = refinements.getGlobal(interned.ip_idx) orelse {
                    std.debug.panic("{s}: interned allocator ip_idx={d} not registered as global", .{ panic_name, interned.ip_idx });
                };
                const agid = refinements.at(ptr_gid).pointer.to;
                const alloc_ref = refinements.at(agid).allocator;
                break :blk .{ .gid = agid, .type_id = alloc_ref.type_id };
            },
            .fnptr => null,
        };
    }

    fn freeSliceLike(state: State, allocator_src: tag.Src, slice_src: tag.Src, allow_interned_noop: bool) !AllocatorIdentity {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        const ptr_idx: Gid = switch (slice_src) {
            .inst => |inst| results[inst].refinement orelse return error.InvalidReallocInput,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse {
                if (allow_interned_noop) return error.InternedNoOp;
                return reportFreeGlobalMemory(ctx);
            },
            .fnptr => {
                if (allow_interned_noop) return error.InternedNoOp;
                return reportFreeGlobalMemory(ctx);
            },
        };
        const ptr_refinement = refinements.at(ptr_idx);
        if (ptr_refinement.* != .pointer) return error.InvalidReallocInput;

        if (ptr_refinement.pointer.analyte.memory_safety) |ptr_ms| {
            switch (ptr_ms) {
                .allocated => |a| {
                    if (a.root_gid != null) return reportFreeFieldPointer(ctx, a);
                },
                .stack => |s| {
                    if (s.root_gid != null) return reportFreeFieldPointerStack(ctx, s);
                },
                .interned => return reportFreeGlobalMemory(ctx),
                .error_stub => return error.InternedNoOp,
                .placeholder => std.debug.panic("placeholder in free-like path - pointer not initialized", .{}),
            }
        }

        const pointee_idx = ptr_refinement.pointer.to;
        const pointee_ref = refinements.at(pointee_idx);
        const pointee_analyte: *Analyte = switch (pointee_ref.*) {
            .region => |*r| blk: {
                if (r.analyte.memory_safety) |*ms| {
                    switch (ms.*) {
                        .allocated => break :blk &r.analyte,
                        .stack => |sp| return reportFreeStackMemory(ctx, sp),
                        .interned => {
                            if (allow_interned_noop) return error.InternedNoOp;
                            return reportFreeGlobalMemory(ctx);
                        },
                        .error_stub => return error.InternedNoOp,
                        .placeholder => std.debug.panic("placeholder in free-like path - region not initialized", .{}),
                    }
                }
                const element_ref = refinements.at(r.to);
                const element_analyte = getAnalytePtr(element_ref);
                if (element_analyte.memory_safety) |element_ms| {
                    if (element_ms == .allocated) {
                        return reportMethodMismatch(ctx, element_ms.allocated, false, true);
                    }
                }
                return error.InvalidReallocInput;
            },
            .scalar => |*s| blk: {
                if (s.analyte.memory_safety) |ms| {
                    if (ms == .allocated) {
                        return reportMethodMismatch(ctx, ms.allocated, false, true);
                    }
                }
                break :blk &s.analyte;
            },
            else => return error.InvalidReallocInput,
        };

        const ms_ptr = &(pointee_analyte.memory_safety orelse return error.InvalidReallocInput);
        switch (ms_ptr.*) {
            .stack => |sp| return reportFreeStackMemory(ctx, sp),
            .interned => return reportFreeGlobalMemory(ctx),
            .error_stub => return error.InternedNoOp,
            .placeholder => std.debug.panic("placeholder in free-like path - pointee not initialized", .{}),
            .allocated => {},
        }

        const a = ms_ptr.allocated;
        if (a.freed) |previous_free| return reportDoubleFree(ctx, a, previous_free);

        const alloc_id = resolveAllocatorIdentity(results, refinements, allocator_src, "allocator_free") orelse return error.InvalidReallocInput;
        if (a.allocator_gid != alloc_id.gid) {
            if (a.type_id != 0 or alloc_id.type_id != 0) {
                if (a.type_id != alloc_id.type_id) {
                    return reportMismatchedAllocator(ctx, a, alloc_id.type_id);
                }
            }
        }

        const slice_inst: ?usize = switch (slice_src) {
            .inst => |inst| inst,
            else => null,
        };
        const free_meta: Free = .{
            .meta = ctx.meta,
            .name_at_free = if (slice_inst) |inst| ctx.buildPathName(results, refinements, inst) else null,
        };
        setFreedRecursive(refinements, pointee_idx, free_meta);
        return alloc_id;
    }

    fn paintAllocatedSliceResult(state: State, index: usize, allocator_gid: Gid, type_id: u32) !void {
        const refinements = state.refinements;
        const result_idx = state.results[index].refinement orelse return;
        const result_ref = refinements.at(result_idx);

        const new_ptr_idx: Gid = switch (result_ref.*) {
            .errorunion => |eu| blk: {
                refinements.at(result_idx).errorunion.analyte.memory_safety = .{ .stack = .{
                    .meta = state.ctx.meta,
                    .root_gid = null,
                } };
                break :blk eu.to;
            },
            .optional => |opt| blk: {
                refinements.at(result_idx).optional.analyte.memory_safety = .{ .stack = .{
                    .meta = state.ctx.meta,
                    .root_gid = null,
                } };
                break :blk opt.to;
            },
            else => return,
        };

        const new_ptr_ref = refinements.at(new_ptr_idx);
        if (new_ptr_ref.* != .pointer) return;
        const new_region_idx = new_ptr_ref.pointer.to;
        const new_region_ref = refinements.at(new_region_idx);
        if (new_region_ref.* != .region) return;
        const new_region = &new_region_ref.region;
        const new_element_ref = new_region.to;

        new_region.analyte.memory_safety = .{ .allocated = .{
            .meta = state.ctx.meta,
            .allocator_gid = allocator_gid,
            .type_id = type_id,
            .root_gid = null,
        } };

        paintSpatialMemory(refinements, new_element_ref, .{ .allocated = .{
            .meta = state.ctx.meta,
            .allocator_gid = allocator_gid,
            .type_id = type_id,
            .root_gid = null,
        } });
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
                .error_stub => return,
                .placeholder => std.debug.panic("placeholder in free - pointer not initialized", .{}),
            }
        }

        // Get the pointee entity
        const pointee_idx = ptr_refinement.pointer.to;
        const pointee_ref = refinements.at(pointee_idx);

        // For free, we expect a region (slice allocation).
        // If pointee is not a region, it's a single-item allocation → mismatch.
        // If pointee is a region:
        //   - If region has .allocated, it's a real alloc allocation → OK
        //   - If region has NO .allocated but element does, it's create wrapped by bitcast → mismatch
        const pointee_analyte: *Analyte = switch (pointee_ref.*) {
            .region => |*r| blk: {
                // Check if region has allocation metadata
                if (r.analyte.memory_safety) |*ms| {
                    switch (ms.*) {
                        .allocated => break :blk &r.analyte, // Real alloc allocation
                        .stack => |sp| return reportFreeStackMemory(ctx, sp),
                        .interned => {
                            // TODO: Ideally we'd distinguish between:
                            // - Empty interned slices (like ArrayList.init items) - OK to free (no-op)
                            // - Non-empty interned slices (like string literals) - error
                            // For now, allow all interned free since empty slices are common
                            // and freeing them is a no-op in most allocators.
                            return;
                        },
                        .error_stub => return,
                        .placeholder => std.debug.panic("placeholder in free - region not initialized", .{}),
                    }
                }
                // Region has no metadata - check if element has allocation metadata
                // (This happens when create allocation is bitcast to [*]T)
                const element_ref = refinements.at(r.to);
                const element_analyte = getAnalytePtr(element_ref);
                if (element_analyte.memory_safety) |element_ms| {
                    if (element_ms == .allocated) {
                        // Element has allocation metadata but region doesn't
                        // This means it was a create allocation wrapped by bitcast → mismatch
                        return reportMethodMismatch(ctx, element_ms.allocated, false, true);
                    }
                }
                // No allocation metadata found - can't verify
                return;
            },
            .scalar => |*s| blk: {
                // Scalar pointee means single-item allocation → mismatch for free
                if (s.analyte.memory_safety) |ms| {
                    if (ms == .allocated) {
                        return reportMethodMismatch(ctx, ms.allocated, false, true);
                    }
                }
                break :blk &s.analyte;
            },
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
            .error_stub => return,
            .placeholder => std.debug.panic("placeholder in free - pointee not initialized", .{}),
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
        // Skip check if both type_ids are 0 (unknown) - can't verify without type info.
        // This happens when allocators are passed through function arguments without
        // going through MkAllocator, causing each load to create a new GID.
        if (free_allocator_gid) |fag| {
            if (a.allocator_gid != fag) {
                // GIDs differ - check if we can verify via type_id
                if (a.type_id != 0 or free_type_id != 0) {
                    if (a.type_id != free_type_id) {
                        return reportMismatchedAllocator(ctx, a, free_type_id);
                    }
                }
            }
        }

        // Note: Method mismatch is already checked above when getting pointee_analyte

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
    /// Marks the old slice as freed (for realloc) and the new slice as allocated.
    /// For remap, old memory is not freed - it just attempts to resize in place.
    fn handleAllocRealloc(state: State, index: usize, args: []const tag.Src, is_remap: bool) !void {
        if (args.len < 2) return;
        const alloc_id = freeSliceLike(state, args[0], args[1], is_remap) catch |err| switch (err) {
            error.InternedNoOp, error.InvalidReallocInput => return,
            else => return err,
        };
        try paintAllocatedSliceResult(state, index, alloc_id.gid, alloc_id.type_id);
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
            paintSpatialMemory(refinements, gid, .{ .stack = .{ .meta = comptime_interned_meta, .root_gid = null } });
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
        // At deinit time, we match by type_id since valueCopy creates new GIDs.
        alloc_ref.allocator.arena_gid = arena_struct_gid;
    }
};

// =========================================================================
// Validation
// =========================================================================

const debug = @import("builtin").mode == .Debug;

/// Validates that memory_safety is correctly set on refinements.
/// - MUST EXIST: .scalar, .pointer, .optional, .errorunion, .region, .recursive, .struct, .union, .fnptr, .allocator
/// - NO ANALYTE: .void, .noreturn, .unimplemented
pub fn testValid(refinement: Refinements.Refinement, idx: usize) void {
    if (!debug) return;

    switch (refinement) {
        .scalar => |s| {
            if (s.analyte.memory_safety == null) std.debug.panic("memory_safety must be set on scalar (idx={d})", .{idx});
        },
        .pointer => |p| {
            if (p.analyte.memory_safety == null) std.debug.panic("memory_safety must be set on pointer (idx={d})", .{idx});
        },
        .fnptr => |f| {
            if (f.analyte.memory_safety == null) std.debug.panic("memory_safety must be set on fnptr (idx={d})", .{idx});
        },
        .allocator => |a| {
            if (a.analyte.memory_safety == null) std.debug.panic("memory_safety must be set on allocator (idx={d})", .{idx});
        },
        // Container types - no undefined_safety on themselves, just check memory_safety exists
        inline .optional, .errorunion, .region, .recursive, .@"struct", .@"union" => |data, t| {
            if (data.analyte.memory_safety == null) std.debug.panic("memory_safety must be set on {s} (idx={d})", .{ @tagName(t), idx });
        },
        // NO ANALYTE - trivial types
        .void, .noreturn, .unimplemented => {},
    }
}
