const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const core = @import("../core.zig");
const Meta = core.Meta;
const tag = @import("../tag.zig");
const gates = @import("gates.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// NOTE: For .optional, .errorunion and .struct refinements, the top-level analyte.undefined should always be null.
// We don't track undefined state on the container itself - only on the payload/fields.

pub const UndefinedSafety = union(enum) {
    defined: void,
    undefined: struct {
        meta: Meta,
        name_when_set: ?[]const u8 = null, // Full path name (arena-allocated at store time)
    },
    inconsistent: struct {
        undefined_meta: Meta, // where undefined was set
        branch_meta: Meta, // where the conditional branch occurred
        name_when_set: ?[]const u8 = null, // Full path name (arena-allocated at store time)
    },

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(&.{@intFromEnum(self)});
    }

    pub fn reportUseBeforeAssign(self: @This(), ctx: *Context) anyerror!void {
        try ctx.meta.print(ctx.writer, "use of undefined value found in ", .{});
        switch (self) {
            .undefined => |p| {
                if (p.name_when_set) |name| {
                    try p.meta.print(ctx.writer, "undefined value assigned to '{s}' in ", .{name});
                } else {
                    try p.meta.print(ctx.writer, "undefined value assigned in ", .{});
                }
            },
            .defined => unreachable,
            .inconsistent => unreachable, // use reportInconsistentBranches instead
        }
        return error.UseBeforeAssign;
    }

    /// Check if a refinement represents an unfilled return slot (has undefined state).
    /// Used by ret_safe to determine if a typed return slot can be overwritten.
    pub fn isUnfilledReturnSlot(refinements: *Refinements, gid: Gid) bool {
        const ref = refinements.at(gid);
        return switch (ref.*) {
            .scalar => |s| if (s.analyte.undefined_safety) |u| u == .undefined else false,
            .pointer => |p| if (p.analyte.undefined_safety) |u| u == .undefined else false,
            else => false,
        };
    }

    pub fn reportInconsistentBranches(self: @This(), ctx: *Context) anyerror!void {
        try ctx.meta.print(ctx.writer, "use of value that may be undefined in ", .{});
        switch (self) {
            .inconsistent => |p| {
                try p.branch_meta.print(ctx.writer, "conditional branch has conflicting status at ", .{});
                if (p.name_when_set) |name| {
                    try p.undefined_meta.print(ctx.writer, "variable '{s}' was set to undefined in ", .{name});
                } else {
                    try p.undefined_meta.print(ctx.writer, "value set to undefined in ", .{});
                }
            },
            .defined => unreachable,
            .undefined => unreachable, // use reportUseBeforeAssign instead
        }
        return error.InconsistentBranches;
    }

    pub fn alloc(state: State, index: usize, params: tag.Alloc) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }

    pub fn alloc_create(state: State, index: usize, params: tag.AllocCreate) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // Result is errorunion -> ptr -> pointee
        const eu_idx = results[index].refinement.?;
        const ptr_idx = refinements.at(eu_idx).errorunion.to;
        // The pointer itself is defined (it exists)
        refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }

    /// Handle allocator.alloc() - set undefined state for slice allocation
    /// Result structure: errorunion → pointer → region → element
    pub fn alloc_alloc(state: State, index: usize, params: tag.AllocAlloc) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // Result is errorunion -> pointer -> region -> element
        const eu_idx = results[index].refinement.?;
        const ptr_idx = refinements.at(eu_idx).errorunion.to;
        // The pointer itself is defined (the slice exists)
        refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
        // The region is a container type - don't set undefined state on it
        const region_idx = refinements.at(ptr_idx).pointer.to;
        // The elements start as undefined (must be set before use)
        const element_idx = refinements.at(region_idx).region.to;
        setUndefinedRecursive(refinements, element_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }

    /// Handle allocator.realloc()/remap() - set undefined state for reallocation
    /// Result structure: errorunion → pointer → region → element
    /// Since realloc preserves data from the original slice, elements are defined
    pub fn alloc_realloc(state: State, index: usize, params: tag.AllocRealloc) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // Result is errorunion -> pointer -> region -> element
        const eu_idx = results[index].refinement.?;
        const ptr_idx = refinements.at(eu_idx).errorunion.to;
        // The pointer itself is defined (the slice exists)
        refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
        // The region is a container type - don't set undefined state on it
        const region_idx = refinements.at(ptr_idx).pointer.to;
        // The elements are defined since realloc preserves data from the original
        const element_idx = refinements.at(region_idx).region.to;
        setUndefinedRecursive(refinements, element_idx, .{ .defined = {} });
    }

    pub fn struct_field_ptr(state: State, index: usize, params: tag.StructFieldPtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid field)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined_safety = .{ .defined = {} };
        // For existing field entities, preserve their undefined state.
        // For newly created entities (from inactive union access), initialize to defined.
        // This handles the case where variant_safety will report an error but we need
        // valid undefined state on the entity for testValid.
        ensureUndefinedStateSet(refinements, ptr.to);
    }

    pub fn ptr_elem_ptr(state: State, index: usize, params: tag.PtrElemPtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid element)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined_safety = .{ .defined = {} };
        // The element's undefined state is already set (it's the shared region element)
        // Don't call ensureUndefinedStateSet - the element already has its state from alloc
    }

    pub fn field_parent_ptr(state: State, index: usize, params: tag.FieldParentPtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid container)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined_safety = .{ .defined = {} };
        // Ensure the container has undefined state set
        ensureUndefinedStateSet(refinements, ptr.to);
    }

    pub fn errunion_payload_ptr_set(state: State, index: usize, params: tag.ErrunionPayloadPtrSet) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid payload)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined_safety = .{ .defined = {} };
        // The payload's undefined state is already set from the error union
    }

    pub fn slice_ptr(state: State, index: usize, params: tag.SlicePtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid region)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined_safety = .{ .defined = {} };
        // The region's undefined state is already set (from alloc)
    }

    /// Ensure an entity and its children have undefined state set.
    /// Only sets state if currently null (newly created), does not overwrite existing state.
    fn ensureUndefinedStateSet(refinements: *Refinements, idx: Gid) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                if (s.analyte.undefined_safety == null) {
                    s.analyte.undefined_safety = .{ .defined = {} };
                }
            },
            .pointer => |*p| {
                if (p.analyte.undefined_safety == null) {
                    p.analyte.undefined_safety = .{ .defined = {} };
                }
                ensureUndefinedStateSet(refinements, p.to);
            },
            .optional => |o| ensureUndefinedStateSet(refinements, o.to),
            .errorunion => |e| ensureUndefinedStateSet(refinements, e.to),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    ensureUndefinedStateSet(refinements, field_idx);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        ensureUndefinedStateSet(refinements, field_idx);
                    }
                }
            },
            .allocator => |*a| {
                if (a.analyte.undefined_safety == null) {
                    a.analyte.undefined_safety = .{ .defined = {} };
                }
            },
            .void, .unimplemented, .noreturn => {},
            .region => |r| {
                // Don't set analyte.undefined on region container - only the uniform element carries undefined state
                ensureUndefinedStateSet(refinements, r.to);
            },
            .recursive => |r| {
                // Follow the recursive reference
                ensureUndefinedStateSet(refinements, r.to);
            },
            .fnptr => |*f| {
                // fnptr is a value type that tracks undefined state
                if (f.analyte.undefined_safety == null) {
                    f.analyte.undefined_safety = .{ .defined = {} };
                }
            },
        }
    }

    /// Get the pointee GID from a source reference by following the pointer.
    /// Returns null if the source is an interned constant, has no refinement, or isn't a pointer.
    fn getPointeeFromSrc(src: tag.Src, results: []const Inst, refinements: *Refinements) ?Gid {
        const ptr_gid: Gid = switch (src) {
            .inst => |ptr| results[ptr].refinement orelse return null,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return null,
            .fnptr => return null,
        };
        return switch (refinements.at(ptr_gid).*) {
            .pointer => |p| p.to,
            else => null,
        };
    }

    pub fn ret_ptr(state: State, index: usize, params: tag.RetPtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
        // The pointee starts as undefined (must be set before ret_load)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }

    /// Block creates placeholder entities from type info. Set their undefined state.
    /// The actual block value comes from br instructions which may override the refinement.
    pub fn block(state: State, index: usize, params: tag.Block) !void {
        _ = params;
        const result_idx = state.results[index].refinement orelse return;
        ensureUndefinedStateSet(state.refinements, result_idx);
    }

    pub fn set_union_tag(state: State, index: usize, params: tag.SetUnionTag) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        // Get the union pointer's refinement based on source type
        const ptr_ref: Gid = switch (params.ptr) {
            .inst => |inst| results[inst].refinement.?,
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse return,
            .fnptr => return, // comptime constant - no undefined tracking
        };
        const container_idx = refinements.at(ptr_ref).pointer.to;
        const u = &refinements.at(container_idx).@"union";

        // When a tag is set, the union container is being initialized - mark as defined
        u.analyte.undefined_safety = .{ .defined = {} };

        // The newly activated field is undefined (tag set but value not stored yet)
        const field_idx = params.field_index.?;
        if (u.fields[field_idx]) |field_eidx| {
            setUndefinedRecursive(refinements, field_eidx, .{ .undefined = .{ .meta = state.ctx.meta } });
        }
    }

    pub fn get_union_tag(state: State, index: usize, params: tag.GetUnionTag) !void {
        const results = state.results;
        const refinements = state.refinements;

        // Get the union we're reading the tag from
        const operand = params.operand orelse {
            // No operand (interned) - tag is defined
            const result_idx = results[index].refinement.?;
            refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
            return;
        };

        const union_ref = results[operand].refinement orelse {
            // No refinement on operand - assume defined
            const result_idx = results[index].refinement.?;
            refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
            return;
        };

        // Check if the union is defined
        const union_refinement = refinements.at(union_ref);
        if (union_refinement.* != .@"union") {
            std.debug.panic("get_union_tag: expected union, got {s}", .{@tagName(union_refinement.*)});
        }

        const union_undefined = union_refinement.@"union".analyte.undefined_safety;
        const result_idx = results[index].refinement.?;

        if (union_undefined) |undef_state| {
            // Propagate undefined state from union to tag
            refinements.at(result_idx).scalar.analyte.undefined_safety = undef_state;
        } else {
            // Union has no undefined tracking - assume defined
            refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
        }
    }

    // Simple operations produce defined scalar results
    pub const bit_and = markResultDefined;
    pub const cmp_eq = markResultDefined;
    pub const cmp_gt = markResultDefined;
    pub const cmp_gte = markResultDefined;
    pub const cmp_lt = markResultDefined;
    pub const cmp_lte = markResultDefined;
    pub const ctz = markResultDefined;
    pub const slice_len = markResultDefined;
    pub const sub = markResultDefined;
    pub const add = markResultDefined;
    pub const is_non_err = markResultDefined;
    pub const unwrap_errunion_err = markResultDefined;
    pub const is_named_enum_value = markResultDefined;
    pub const alloc_resize = markResultDefined; // resize returns a defined bool

    // Null checks produce defined boolean results
    pub const is_non_null = markResultDefined;
    pub const is_null = markResultDefined;
    pub const is_non_null_ptr = markResultDefined;
    pub const is_null_ptr = markResultDefined;

    // Overflow operations produce a struct with defined fields
    pub fn add_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        markStructFieldsDefined(state.results, index, state.refinements);
    }

    pub fn sub_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        markStructFieldsDefined(state.results, index, state.refinements);
    }

    pub fn mul_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        markStructFieldsDefined(state.results, index, state.refinements);
    }

    /// aggregate_init creates a struct/array from element values.
    /// Copy undefined_safety state from each source element to the corresponding field.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy undefined_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i >= params.elements.len) break;
                    const src = params.elements[i];
                    copyUndefinedState(state, field_gid, src);
                }
            },
            .region => |r| {
                // For arrays/regions: use uniform model - first element applies to all
                if (params.elements.len > 0) {
                    copyUndefinedState(state, r.to, params.elements[0]);
                }
            },
            else => {},
        }
    }

    /// Copy undefined_safety state from a source to a destination refinement.
    fn copyUndefinedState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values are comptime - always defined
            .fnptr => null, // Function pointers are always defined
        };

        // For interned/fnptr sources, mark destination as defined
        if (src_gid == null) {
            setDefinedRecursive(state.refinements, dst_gid);
            return;
        }

        // Copy analyte state from source to destination
        const src_ref = state.refinements.at(src_gid.?);
        const dst_ref = state.refinements.at(dst_gid);

        // Get undefined_safety from source based on its type
        const src_undef: ?UndefinedSafety = switch (src_ref.*) {
            .scalar => |s| s.analyte.undefined_safety,
            .pointer => |p| p.analyte.undefined_safety,
            .allocator => |a| a.analyte.undefined_safety,
            .fnptr => |f| f.analyte.undefined_safety,
            // Container types don't have undefined state on themselves
            .optional, .errorunion, .@"struct", .@"union", .region, .recursive, .void, .noreturn, .unimplemented => null,
        };

        // Set undefined_safety on destination based on its type
        switch (dst_ref.*) {
            .scalar => |*s| s.analyte.undefined_safety = src_undef orelse .{ .defined = {} },
            .pointer => |*p| p.analyte.undefined_safety = src_undef orelse .{ .defined = {} },
            .allocator => |*a| a.analyte.undefined_safety = src_undef orelse .{ .defined = {} },
            .fnptr => |*f| f.analyte.undefined_safety = src_undef orelse .{ .defined = {} },
            // For container types in destination, recurse
            .optional => |o| copyUndefinedStateRecursive(state.refinements, o.to, src_gid.?),
            .errorunion => |e| copyUndefinedStateRecursive(state.refinements, e.to, src_gid.?),
            .@"struct" => |s| {
                // Copy to matching struct fields
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyUndefinedStateRecursive(state.refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .@"union" => |u| {
                // Copy to matching union variants
                const src_u = src_ref.@"union";
                for (u.fields, 0..) |maybe_field, i| {
                    const field_gid = maybe_field orelse continue;
                    if (i < src_u.fields.len) {
                        if (src_u.fields[i]) |src_field| {
                            copyUndefinedStateRecursive(state.refinements, field_gid, src_field);
                        }
                    }
                }
            },
            .region => |r| copyUndefinedStateRecursive(state.refinements, r.to, src_gid.?),
            .recursive, .void, .noreturn, .unimplemented => {},
        }
    }

    /// Recursively copy undefined_safety state from source GID to destination GID.
    fn copyUndefinedStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        // Get and set analyte state based on types
        switch (dst_ref.*) {
            .scalar => |*s| s.analyte.undefined_safety = switch (src_ref.*) {
                .scalar => |ss| ss.analyte.undefined_safety orelse .{ .defined = {} },
                else => .{ .defined = {} },
            },
            .pointer => |*p| {
                p.analyte.undefined_safety = switch (src_ref.*) {
                    .pointer => |sp| sp.analyte.undefined_safety orelse .{ .defined = {} },
                    else => .{ .defined = {} },
                };
                // Also copy pointee state
                if (src_ref.* == .pointer) {
                    copyUndefinedStateRecursive(refinements, p.to, src_ref.pointer.to);
                }
            },
            .optional => |o| {
                if (src_ref.* == .optional) {
                    copyUndefinedStateRecursive(refinements, o.to, src_ref.optional.to);
                }
            },
            .errorunion => |e| {
                if (src_ref.* == .errorunion) {
                    copyUndefinedStateRecursive(refinements, e.to, src_ref.errorunion.to);
                }
            },
            .@"struct" => |s| {
                if (src_ref.* == .@"struct") {
                    const src_s = src_ref.@"struct";
                    for (s.fields, 0..) |field_gid, i| {
                        if (i < src_s.fields.len) {
                            copyUndefinedStateRecursive(refinements, field_gid, src_s.fields[i]);
                        }
                    }
                }
            },
            .@"union" => |u| {
                if (src_ref.* == .@"union") {
                    const src_u = src_ref.@"union";
                    for (u.fields, 0..) |maybe_field, i| {
                        const field_gid = maybe_field orelse continue;
                        if (i < src_u.fields.len) {
                            if (src_u.fields[i]) |src_field| {
                                copyUndefinedStateRecursive(refinements, field_gid, src_field);
                            }
                        }
                    }
                }
            },
            .allocator => |*a| a.analyte.undefined_safety = switch (src_ref.*) {
                .allocator => |sa| sa.analyte.undefined_safety orelse .{ .defined = {} },
                else => .{ .defined = {} },
            },
            .fnptr => |*f| f.analyte.undefined_safety = switch (src_ref.*) {
                .fnptr => |sf| sf.analyte.undefined_safety orelse .{ .defined = {} },
                else => .{ .defined = {} },
            },
            .region => |r| {
                if (src_ref.* == .region) {
                    copyUndefinedStateRecursive(refinements, r.to, src_ref.region.to);
                }
            },
            .recursive, .void, .noreturn, .unimplemented => {},
        }
    }

    fn markResultDefined(state: State, index: usize, params: anytype) !void {
        _ = params;
        const result_idx = state.results[index].refinement.?;
        state.refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
    }

    fn markStructFieldsDefined(results: []Inst, index: usize, refinements: *Refinements) void {
        const result_idx = results[index].refinement.?;
        const s = refinements.at(result_idx).@"struct";
        for (s.fields) |field_idx| {
            refinements.at(field_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
        }
    }

    /// Helper to recursively set all scalars/pointers in a refinement tree to defined.
    /// Preserves existing undefined states (only sets to defined if currently null).
    /// This allows per-field undefined tracking for structs like .{ .x = 42, .y = undefined }.
    pub fn setDefinedRecursive(refinements: *Refinements, idx: Gid) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                // Only set to defined if not already set (preserve existing undefined state)
                if (s.analyte.undefined_safety == null) {
                    s.analyte.undefined_safety = .{ .defined = {} };
                }
            },
            .pointer => |*p| {
                // Only set to defined if not already set (preserve existing undefined state)
                if (p.analyte.undefined_safety == null) {
                    p.analyte.undefined_safety = .{ .defined = {} };
                }
                setDefinedRecursive(refinements, p.to);
            },
            .optional => |o| setDefinedRecursive(refinements, o.to),
            .errorunion => |e| setDefinedRecursive(refinements, e.to),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    setDefinedRecursive(refinements, field_idx);
                }
            },
            .@"union" => |*u| {
                // Set union's analyte to defined
                u.analyte.undefined_safety = .{ .defined = {} };
                // NOTE: Do NOT recurse into union fields. Union fields have their own
                // independent undefined states based on whether the field VALUE is undefined
                // (e.g., .{ .int = undefined } has a defined union but undefined field value).
            },
            .allocator => |*a| {
                // Only set to defined if not already set
                if (a.analyte.undefined_safety == null) {
                    a.analyte.undefined_safety = .{ .defined = {} };
                }
            },
            .void, .unimplemented, .noreturn => {},
            .region => |r| setDefinedRecursive(refinements, r.to),
            .recursive => |r| setDefinedRecursive(refinements, r.to),
            .fnptr => |*f| {
                // Only set to defined if not already set (preserve existing undefined state)
                if (f.analyte.undefined_safety == null) {
                    f.analyte.undefined_safety = .{ .defined = {} };
                }
            },
        }
    }

    /// Helper to FORCE all scalars/pointers in a refinement tree to defined.
    /// Unlike setDefinedRecursive, this always overwrites existing undefined states.
    /// Used when explicitly storing a value (like .null to an optional).
    fn forceDefinedRecursive(refinements: *Refinements, idx: Gid) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
            .pointer => |*p| {
                p.analyte.undefined_safety = .{ .defined = {} };
                forceDefinedRecursive(refinements, p.to);
            },
            .optional => |o| forceDefinedRecursive(refinements, o.to),
            .errorunion => |e| forceDefinedRecursive(refinements, e.to),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    forceDefinedRecursive(refinements, field_idx);
                }
            },
            .@"union" => |*u| {
                u.analyte.undefined_safety = .{ .defined = {} };
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        forceDefinedRecursive(refinements, field_idx);
                    }
                }
            },
            .allocator => |*a| a.analyte.undefined_safety = .{ .defined = {} },
            .fnptr => |*f| f.analyte.undefined_safety = .{ .defined = {} },
            .void, .unimplemented, .noreturn => {},
            .region => |r| forceDefinedRecursive(refinements, r.to),
            .recursive => |r| forceDefinedRecursive(refinements, r.to),
        }
    }

    /// Recursively set name_when_set on undefined states that don't have a name yet.
    /// Called by DbgVarPtr to retroactively name undefined states that were created
    /// before the variable name was known (since dbg_var_ptr comes AFTER stores in AIR).
    pub fn setNameOnUndefined(refinements: *Refinements, idx: Gid, name: ?[]const u8) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                if (s.analyte.undefined_safety) |*undef| {
                    if (undef.* == .undefined and undef.undefined.name_when_set == null) {
                        undef.undefined.name_when_set = name;
                    }
                }
            },
            .pointer => |*p| {
                if (p.analyte.undefined_safety) |*undef| {
                    if (undef.* == .undefined and undef.undefined.name_when_set == null) {
                        undef.undefined.name_when_set = name;
                    }
                }
                setNameOnUndefined(refinements, p.to, name);
            },
            .optional => |o| setNameOnUndefined(refinements, o.to, name),
            .errorunion => |e| setNameOnUndefined(refinements, e.to, name),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    setNameOnUndefined(refinements, field_idx, name);
                }
            },
            else => {},
        }
    }

    /// Force-set name_when_set on undefined states, overwriting any existing name.
    /// Used by DbgVarVal when a value flows from one variable to another - the new
    /// variable's name should be used in error messages, not the original source.
    fn forceNameOnUndefined(refinements: *Refinements, idx: Gid, name: ?[]const u8) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                if (s.analyte.undefined_safety) |*undef| {
                    if (undef.* == .undefined) {
                        undef.undefined.name_when_set = name;
                    }
                }
            },
            .pointer => |*p| {
                if (p.analyte.undefined_safety) |*undef| {
                    if (undef.* == .undefined) {
                        undef.undefined.name_when_set = name;
                    }
                }
                forceNameOnUndefined(refinements, p.to, name);
            },
            .optional => |o| forceNameOnUndefined(refinements, o.to, name),
            .errorunion => |e| forceNameOnUndefined(refinements, e.to, name),
            .@"struct" => |s| {
                for (s.fields) |field_idx| {
                    forceNameOnUndefined(refinements, field_idx, name);
                }
            },
            else => {},
        }
    }

    /// Retroactively set variable name on undefined states.
    /// The name is already set on the instruction by DbgVarPtr.apply().
    pub fn dbg_var_ptr(state: State, index: usize, params: tag.DbgVarPtrParams) !void {
        _ = index;
        const inst = params.ptr orelse return;
        const ptr_idx = state.results[inst].refinement orelse return;
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = state.refinements.at(ptr_idx).pointer.to;
        // Build the full path name and set on undefined states
        const name = state.ctx.buildPathName(state.results, state.refinements, inst);
        setNameOnUndefined(state.refinements, pointee_idx, name);
    }

    /// Retroactively set variable name on undefined states for value variables.
    /// Unlike dbg_var_ptr which follows a pointer, this operates on the value directly.
    /// Uses forceNameOnUndefined to overwrite any existing name since the value may have
    /// flowed from another variable (e.g., `const y = x` - error should say 'y', not 'x').
    pub fn dbg_var_val(state: State, index: usize, params: tag.DbgVarValParams) !void {
        _ = index;
        const inst = params.ptr orelse return;
        const value_idx = state.results[inst].refinement orelse return;
        // Use the name_id directly - don't use buildPathName which would follow
        // the load back to its source pointer and get the wrong name
        const name = state.ctx.getName(params.name_id);
        forceNameOnUndefined(state.refinements, value_idx, name);
    }

    pub fn optional_payload(state: State, index: usize, params: tag.OptionalPayload) !void {
        _ = state;
        _ = index;
        // For .inst: result shares source's entity, undefined state already correct
        if (params.src == .interned) {
            @panic("optional_payload: interned source unimplemented");
        }
    }

    /// optional_payload_ptr creates a new pointer to the payload.
    /// The pointer itself is defined; the payload keeps its existing state.
    pub fn optional_payload_ptr(state: State, index: usize, params: tag.OptionalPayloadPtr) !void {
        _ = params;
        const ptr_idx = state.results[index].refinement orelse return;
        state.refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
    }

    /// unwrap_errunion_payload_ptr creates a new pointer to the error union payload.
    /// The pointer itself is defined; the payload keeps its existing state.
    pub fn unwrap_errunion_payload_ptr(state: State, index: usize, params: tag.UnwrapErrunionPayloadPtr) !void {
        _ = params;
        const ptr_idx = state.results[index].refinement orelse return;
        state.refinements.at(ptr_idx).pointer.analyte.undefined_safety = .{ .defined = {} };
    }

    /// With unified args, the GID already has proper defined/undefined state
    /// set by srcSliceToGidSlice (interned/fnptr are marked defined there).
    pub fn arg(state: State, index: usize, params: tag.Arg) !void {
        _ = state;
        _ = index;
        _ = params;
        // Nothing to do - state was already set by srcSliceToGidSlice
    }

    /// br sets refinement on its target block, not on itself.
    /// When it creates scalars for the block, we need to set their undefined state.
    pub fn br(state: State, index: usize, params: tag.Br) !void {
        _ = index;
        // br sets refinement on self.block, not on its own index
        // When source is an eidx with existing refinement, undefined state is already set.
        // When br creates a new scalar (interned source), we need to set undefined.
        switch (params.src) {
            .inst => {}, // Source has existing refinement with undefined state
            .interned => |interned| {
                // Check if it's a tracked global with existing refinement
                if (state.refinements.getGlobal(interned.ip_idx) == null) {
                    // Not a tracked global - comptime constant, mark as defined
                    const block_idx = state.results[params.block].refinement orelse return;
                    setDefinedRecursive(state.refinements, block_idx);
                }
            },
            .fnptr => {
                const block_idx = state.results[params.block].refinement orelse return;
                setDefinedRecursive(state.refinements, block_idx);
            },
        }
    }

    /// ret_safe with interned values creates new entities via typeToRefinement.
    /// Those entities need undefined state set - interned values are compile-time constants, so defined.
    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        // Only handle interned non-void returns - those are the ones that create new entities
        switch (params.src) {
            .interned => |interned| {
                // Check if it's a tracked global
                if (state.refinements.getGlobal(interned.ip_idx) != null) {
                    // Tracked global - already has undefined state from initWithGlobals
                    return;
                }
                // Not a tracked global - comptime constant
                if (interned.ty != .void) {
                    // Interned values are compile-time constants, so defined
                    // With global refinements, return_gid points to slot in state.refinements
                    setDefinedRecursive(state.refinements, state.return_gid);
                }
            },
            .fnptr => {
                // Function pointer constants are always defined
                setDefinedRecursive(state.refinements, state.return_gid);
            },
            .inst => {}, // Already has undefined state from callee
        }
    }

    /// Recursively set undefined state on a refinement and its children.
    /// Only called when storing an undefined value (src type is .undefined).
    fn setUndefinedRecursive(refinements: *Refinements, idx: Gid, undef_state: UndefinedSafety) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.analyte.undefined_safety = undef_state,
            .pointer => |*p| {
                p.analyte.undefined_safety = undef_state;
                setUndefinedRecursive(refinements, p.to, undef_state);
            },
            .optional => |o| {
                // Don't set analyte.undefined on optional - only the payload carries undefined state
                setUndefinedRecursive(refinements, o.to, undef_state);
            },
            .errorunion => |e| {
                // Don't set analyte.undefined on errorunion - only the payload carries undefined state
                setUndefinedRecursive(refinements, e.to, undef_state);
            },
            .@"struct" => |s| {
                // Don't set analyte.undefined on struct - only the fields carry undefined state
                for (s.fields) |field_idx| {
                    setUndefinedRecursive(refinements, field_idx, undef_state);
                }
            },
            .@"union" => |*u| {
                // Set analyte.undefined on union - used when activating inactive fields later
                u.analyte.undefined_safety = undef_state;
                // Also set on active fields
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setUndefinedRecursive(refinements, field_idx, undef_state);
                    }
                }
            },
            .allocator => |*a| a.analyte.undefined_safety = undef_state,
            .void, .unimplemented, .noreturn => {},
            .region => |r| {
                // Don't set analyte.undefined on region - only the uniform element carries undefined state
                setUndefinedRecursive(refinements, r.to, undef_state);
            },
            .recursive => |r| {
                // Follow the recursive reference
                setUndefinedRecursive(refinements, r.to, undef_state);
            },
            .fnptr => |*f| f.analyte.undefined_safety = undef_state,
        }
    }

    /// Apply defined/undefined state from an interned type to a refinement.
    /// Handles field-level undefined for structs where some fields may be undefined.
    /// Uses forceDefinedRecursive because explicit stores should overwrite existing undefined states.
    fn applyInternedType(refinements: *Refinements, idx: Gid, ty: tag.Type, ctx: *Context) void {
        switch (ty) {
            .undefined => {
                // Mark as undefined - don't recurse further, undefined is terminal
                const undef_state: UndefinedSafety = .{ .undefined = .{ .meta = ctx.meta } };
                setUndefinedRecursive(refinements, idx, undef_state);
            },
            .@"struct" => |struct_type| {
                // Apply field-level undefined state
                switch (refinements.at(idx).*) {
                    .@"struct" => |s| {
                        for (struct_type.fields, 0..) |field_type, i| {
                            if (i < s.fields.len) {
                                applyInternedType(refinements, s.fields[i], field_type, ctx);
                            }
                        }
                    },
                    else => {
                        // Non-struct refinement - just mark as defined
                        forceDefinedRecursive(refinements, idx);
                    },
                }
            },
            .scalar => {
                switch (refinements.at(idx).*) {
                    .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                    .pointer => |*p| p.analyte.undefined_safety = .{ .defined = {} },
                    else => {},
                }
            },
            .pointer => |inner| {
                switch (refinements.at(idx).*) {
                    .pointer => |*p| {
                        p.analyte.undefined_safety = .{ .defined = {} };
                        applyInternedType(refinements, p.to, inner.*, ctx);
                    },
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .optional => |inner| {
                switch (refinements.at(idx).*) {
                    .optional => |o| applyInternedType(refinements, o.to, inner.*, ctx),
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .errorunion => |inner| {
                switch (refinements.at(idx).*) {
                    .errorunion => |e| applyInternedType(refinements, e.to, inner.*, ctx),
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .null => {
                // Null value - mark inner as defined (it's explicitly null, not undefined)
                switch (refinements.at(idx).*) {
                    .optional => |o| forceDefinedRecursive(refinements, o.to),
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .@"union" => |union_type| {
                // Apply field-level undefined state for unions
                switch (refinements.at(idx).*) {
                    .@"union" => |*u| {
                        // Mark union itself as defined (since we're storing a value, not undefined)
                        u.analyte.undefined_safety = .{ .defined = {} };
                        for (union_type.variants, 0..) |field_type, i| {
                            if (i < u.fields.len) {
                                if (u.fields[i]) |field_idx| {
                                    applyInternedType(refinements, field_idx, field_type, ctx);
                                }
                            }
                        }
                    },
                    else => {
                        // Non-union refinement - just mark as defined
                        forceDefinedRecursive(refinements, idx);
                    },
                }
            },
            .allocator => {
                // Allocator value - mark as defined
                switch (refinements.at(idx).*) {
                    .allocator => |*a| a.analyte.undefined_safety = .{ .defined = {} },
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .void => {},
            .region => |inner| {
                switch (refinements.at(idx).*) {
                    .region => |r| applyInternedType(refinements, r.to, inner.*, ctx),
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .recursive => {
                // Recursive type reference - the actual structure is materialized elsewhere
                // Just mark as defined like a scalar
                forceDefinedRecursive(refinements, idx);
            },
            .fnptr => {
                // Function pointer - mark as defined
                switch (refinements.at(idx).*) {
                    .fnptr => |*f| f.analyte.undefined_safety = .{ .defined = {} },
                    else => forceDefinedRecursive(refinements, idx),
                }
            },
            .unimplemented => {
                // Skip unimplemented types nested inside structs - nothing to track
            },
        }
    }

    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        // Get pointer GID based on ptr type (like load does)
        const ptr_gid: Gid = switch (params.ptr) {
            .inst => |ptr| results[ptr].refinement orelse @panic("store: ptr inst has no refinement"),
            .interned => |interned| refinements.getGlobal(interned.ip_idx) orelse @panic("store: global not found"),
            .fnptr => @panic("store: storing through constant pointer not supported"),
        };
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = refinements.at(ptr_gid).pointer.to;

        // Check if source is an undefined type (interned with .undefined wrapper)
        const is_undef = switch (params.src) {
            .interned => |interned| interned.ty == .undefined,
            else => false,
        };

        if (is_undef) {
            // Undefined stores: mark pointee and all children as undefined (recursive)
            // Build full path name for the destination pointer
            const name_when_set: ?[]const u8 = switch (params.ptr) {
                .inst => |ptr| state.ctx.buildPathName(results, refinements, ptr),
                .interned => null, // TODO: look up global name from IP index
                .fnptr => null,
            };
            const undef_state: UndefinedSafety = .{ .undefined = .{ .meta = state.ctx.meta, .name_when_set = name_when_set } };
            setUndefinedRecursive(refinements, pointee_idx, undef_state);
        } else {
            // Defined stores: mark the pointee as defined
            // Note: structural .to updates are handled by the Store tag handler
            switch (params.src) {
                .inst => |src_idx| {
                    // Check if source has a refinement
                    _ = results[src_idx].refinement orelse {
                        // Source has no refinement - just mark as defined
                        switch (refinements.at(pointee_idx).*) {
                            .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                            .pointer => |*p| p.analyte.undefined_safety = .{ .defined = {} },
                            else => {},
                        }
                        return;
                    };
                    // Mark the pointee as defined based on its type
                    switch (refinements.at(pointee_idx).*) {
                        .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                        .pointer => |*p| p.analyte.undefined_safety = .{ .defined = {} },
                        .optional => |o| {
                            // Mark the payload as defined
                            switch (refinements.at(o.to).*) {
                                .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                                .pointer => |*p| p.analyte.undefined_safety = .{ .defined = {} },
                                else => {},
                            }
                        },
                        .errorunion => |e| {
                            switch (refinements.at(e.to).*) {
                                .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                                .pointer => |*p| p.analyte.undefined_safety = .{ .defined = {} },
                                else => {},
                            }
                        },
                        .@"struct" => |s| {
                            // When storing a struct value, copy undefined_safety from source fields
                            // The source is also a struct - copy field by field
                            const src_gid = results[src_idx].refinement.?;
                            const src_ref = refinements.at(src_gid);
                            if (src_ref.* == .@"struct") {
                                const src_s = src_ref.@"struct";
                                for (s.fields, 0..) |field_idx, i| {
                                    if (i < src_s.fields.len) {
                                        copyUndefinedStateRecursive(refinements, field_idx, src_s.fields[i]);
                                    }
                                }
                            } else {
                                // Fallback: mark all fields as defined
                                for (s.fields) |field_idx| {
                                    setDefinedRecursive(refinements, field_idx);
                                }
                            }
                        },
                        .@"union" => |*u| {
                            // When storing a union value, mark union and active fields as defined
                            u.analyte.undefined_safety = .{ .defined = {} };
                            for (u.fields) |field_idx_opt| {
                                if (field_idx_opt) |field_idx| {
                                    setDefinedRecursive(refinements, field_idx);
                                }
                            }
                        },
                        .allocator => |*a| a.analyte.undefined_safety = .{ .defined = {} },
                        .fnptr => |*f| f.analyte.undefined_safety = .{ .defined = {} },
                        .void, .unimplemented, .noreturn => {},
                        .region => |r| {
                            // When storing to a region, mark the uniform element as defined
                            setDefinedRecursive(refinements, r.to);
                        },
                        .recursive => |r| {
                            // Follow the recursive reference
                            setDefinedRecursive(refinements, r.to);
                        },
                    }
                },
                .interned => |interned| {
                    // Try to look up as a tracked global first
                    if (refinements.getGlobal(interned.ip_idx)) |global_gid| {
                        // When storing a pointer value, update the destination's `to` field
                        switch (refinements.at(pointee_idx).*) {
                            .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                            .pointer => |*p| {
                                // If global is also a pointer, update .to to share the target
                                if (refinements.at(global_gid).* == .pointer) {
                                    const global_ptr = refinements.at(global_gid).pointer;
                                    p.to = global_ptr.to;
                                    p.analyte.undefined_safety = .{ .defined = {} };
                                } else {
                                    p.analyte.undefined_safety = .{ .defined = {} };
                                }
                            },
                            else => setDefinedRecursive(refinements, pointee_idx),
                        }
                    } else {
                        // Not a tracked global - comptime constant, apply from type
                        applyInternedType(refinements, pointee_idx, interned.ty, state.ctx);
                    }
                },
                .fnptr => {
                    // Function pointer source - mark pointee as defined
                    setDefinedRecursive(refinements, pointee_idx);
                },
            }
        }
    }

    pub fn load(state: State, index: usize, params: tag.Load) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Get pointee GID by following the pointer chain.
        // If we can't follow (interned constant, missing refinement, or not a pointer),
        // the tag handler created a new entity - ensure its undefined state is set.
        const pointee_idx = getPointeeFromSrc(params.ptr, results, refinements) orelse {
            const result_idx = results[index].refinement.?;
            ensureUndefinedStateSet(refinements, result_idx);
            return;
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |s| {
                // undefined is null when value wasn't allocated through our tracked mechanisms
                // (e.g., external data, FFI). No undefined tracking available - skip.
                const undef = s.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.analyte.undefined_safety = .{ .defined = {} };
                    },
                }
            },
            .pointer => |ind| {
                // undefined is null when pointer wasn't allocated through our tracked mechanisms.
                // No undefined tracking available - skip.
                const undef = ind.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // For compound types, we share the entity so state is already correct.
                        // Only propagate for fresh scalars.
                        const idx = results[index].refinement orelse return;
                        switch (refinements.at(idx).*) {
                            .scalar => |*s| s.analyte.undefined_safety = .{ .defined = {} },
                            else => {}, // Shared entity - state is already correct
                        }
                    },
                }
            },
            .optional => |o| {
                // Optional containers don't track undefined - check the payload
                // Recurse to check the payload's undefined state
                switch (refinements.at(o.to).*) {
                    .scalar => |s| {
                        const undef = s.analyte.undefined_safety orelse return;
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    },
                    else => {}, // Other payload types - skip for now
                }
            },
            .errorunion => |e| {
                // Error union containers don't track undefined - check the payload
                switch (refinements.at(e.to).*) {
                    .scalar => |s| {
                        const undef = s.analyte.undefined_safety orelse return;
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    },
                    else => {}, // Other payload types - skip for now
                }
            },
            .@"struct" => {
                // Loading a struct doesn't error - individual field access will check
                // The loaded struct value will carry the field refinements
            },
            .@"union" => {
                // Loading a union doesn't error - individual field access will check
                // The loaded union value will carry the field refinements
            },
            .region => |r| {
                // Region container doesn't track undefined - check the uniform element
                switch (refinements.at(r.to).*) {
                    .scalar => |s| {
                        const undef = s.analyte.undefined_safety orelse return;
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    },
                    else => {}, // Other element types - recurse or skip
                }
            },
            .fnptr => |f| {
                // Function pointer - check for undefined
                const undef = f.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            .allocator => |a| {
                // Allocator - check for undefined
                const undef = a.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            .unimplemented => @panic("load: pointee refinement is unimplemented"),
            else => |t| std.debug.panic("load: unexpected pointee type {s}", .{@tagName(t)}),
        }
    }

    pub fn struct_field_val(state: State, index: usize, params: tag.StructFieldVal) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const result_idx = results[index].refinement.?;

        const operand = params.operand orelse {
            // Interned/global struct - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
            return;
        };
        const container_ref = results[operand].refinement orelse {
            // No struct refinement - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
            return;
        };

        switch (refinements.at(container_ref).*) {
            .@"struct" => |s| {
                // Zig's bounds checking handles out-of-bounds access
                const field_idx = s.fields[params.field_index];

                // When field is in bounds, result shares field's refinement (undefined state already set).
                // Check if this field is undefined and report error if accessed.
                try checkFieldUndefined(refinements, field_idx, ctx);
            },
            .@"union" => |u| {
                // For unions, if the field is inactive (was null in tag handler), it was just created
                // and needs to inherit the union's undefined state
                if (u.fields[params.field_index]) |field_idx| {
                    // Active field - check its state
                    try checkFieldUndefined(refinements, field_idx, ctx);
                } else {
                    // Field was inactive - the tag handler created a fresh entity
                    // Check the union's analyte undefined state instead
                    if (u.analyte.undefined_safety) |undef| {
                        // Set the result's undefined state before checking/reporting errors
                        // This ensures testValid won't panic on the fresh result entity
                        setUndefinedRecursive(refinements, result_idx, undef);
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    } else {
                        // No undefined state on union - set result to defined
                        setDefinedRecursive(refinements, result_idx);
                    }
                }
            },
            else => {
                // Not a struct or union - result was created as fresh scalar, set to defined
                refinements.at(result_idx).scalar.analyte.undefined_safety = .{ .defined = {} };
            },
        }
    }

    fn checkFieldUndefined(refinements: *Refinements, field_idx: Refinements.Gid, ctx: *Context) !void {
        switch (refinements.at(field_idx).*) {
            .scalar => |sc| {
                const undef = sc.analyte.undefined_safety orelse @panic("struct_field_val: field scalar has no undefined state");
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            .pointer => |ind| {
                const undef = ind.analyte.undefined_safety orelse @panic("struct_field_val: field pointer has no undefined state");
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            // Optional and struct containers don't track undefined on themselves
            // Their children carry the undefined state
            .optional, .@"struct", .@"union" => {},
            else => |t| std.debug.panic("struct_field_val: unexpected field refinement type {s}", .{@tagName(t)}),
        }
    }

    // Backward propagation is handled centrally by Inst.backPropagate()

    /// Merge undefined states from N branches for a single node.
    /// Called by tag.splatMerge which handles the tree traversal.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        refinements: *Refinements,
        orig_gid: Gid,
        branches: []const ?State,
        branch_gids: []const ?Gid,
    ) !void {
        _ = merge_tag;
        const orig_ref = refinements.at(orig_gid);

        switch (orig_ref.*) {
            .scalar => |*s| {
                if (mergeUndefinedFromBranches(ctx, branches, branch_gids, getScalarUndefined)) |merged| {
                    s.analyte.undefined_safety = merged;
                }
            },
            .pointer => |*p| {
                if (mergeUndefinedFromBranches(ctx, branches, branch_gids, getPointerUndefined)) |merged| {
                    p.analyte.undefined_safety = merged;
                }
            },
            // No undefined tracking on these container types - recursion handled by tag.zig
            else => {},
        }
    }

    /// Get undefined state from a scalar at given GID
    fn getScalarUndefined(branch: State, branch_gid: Gid) ?UndefinedSafety {
        const ref = branch.refinements.at(branch_gid);
        if (ref.* != .scalar) return null;
        return ref.scalar.analyte.undefined_safety;
    }

    /// Get undefined state from a pointer at given GID
    fn getPointerUndefined(branch: State, branch_gid: Gid) ?UndefinedSafety {
        const ref = branch.refinements.at(branch_gid);
        if (ref.* != .pointer) return null;
        return ref.pointer.analyte.undefined_safety;
    }

    /// Merge undefined states from all reachable branches.
    fn mergeUndefinedFromBranches(
        ctx: *Context,
        branches: []const ?State,
        branch_gids: []const ?Gid,
        comptime getUndefined: fn (State, Gid) ?UndefinedSafety,
    ) ?UndefinedSafety {
        var result: ?UndefinedSafety = null;

        for (branches, branch_gids) |branch_opt, branch_gid_opt| {
            const branch = branch_opt orelse continue;
            const branch_gid = branch_gid_opt orelse continue;
            const branch_undef = getUndefined(branch, branch_gid) orelse continue;

            if (result) |current| {
                result = mergeUndefinedStates(ctx, current, branch_undef);
            } else {
                result = branch_undef;
            }
        }

        return result;
    }

    fn mergeUndefinedStates(ctx: *Context, a: UndefinedSafety, b: UndefinedSafety) UndefinedSafety {
        if (a == .inconsistent) return a;
        if (b == .inconsistent) return b;

        const a_is_defined = a == .defined;
        const b_is_defined = b == .defined;

        if (a_is_defined and b_is_defined) return .{ .defined = {} };
        if (!a_is_defined and !b_is_defined) return a;

        const undef_state = if (!a_is_defined) a.undefined else b.undefined;
        return .{ .inconsistent = .{
            .undefined_meta = undef_state.meta,
            .branch_meta = ctx.meta,
            .name_when_set = undef_state.name_when_set,
        } };
    }

    /// Initialize the undefined state on a return slot refinement.
    /// Return slots start as undefined - they become defined when ret_safe fills them.
    pub fn retval_init(refinements: *Refinements, gid: Gid, ctx: *Context) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.undefined_safety = .{ .undefined = .{ .meta = ctx.meta } };
            },
            .pointer => |p| {
                ref.pointer.analyte.undefined_safety = .{ .undefined = .{ .meta = ctx.meta } };
                retval_init(refinements, p.to, ctx);
            },
            .optional => |o| retval_init(refinements, o.to, ctx),
            .errorunion => |e| retval_init(refinements, e.to, ctx),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    retval_init(refinements, field_gid, ctx);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        retval_init(refinements, field_gid, ctx);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.undefined_safety = .{ .undefined = .{ .meta = ctx.meta } };
            },
            .fnptr => {
                ref.fnptr.analyte.undefined_safety = .{ .undefined = .{ .meta = ctx.meta } };
            },
            .region => |r| {
                // Region is a container type - don't set undefined state on it, just recurse
                retval_init(refinements, r.to, ctx);
            },
            .recursive => {
                // Don't follow recursive references - they point back to an ancestor struct
                // that is already being processed. Following would cause infinite recursion.
            },
            .void => {}, // void return type is valid
            .unimplemented => @panic("retval_init: unimplemented return type"),
            .noreturn => unreachable, // noreturn functions don't return, shouldn't reach here
        }
    }

    /// Initialize the undefined state on a return slot refinement as DEFINED.
    /// Used for interned comptime values (constants, null) that are not the .undefined type.
    pub fn retval_init_defined(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.undefined_safety = .{ .defined = {} };
            },
            .pointer => |p| {
                ref.pointer.analyte.undefined_safety = .{ .defined = {} };
                retval_init_defined(refinements, p.to);
            },
            .optional => |o| retval_init_defined(refinements, o.to),
            .errorunion => |e| retval_init_defined(refinements, e.to),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    retval_init_defined(refinements, field_gid);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        retval_init_defined(refinements, field_gid);
                    }
                }
            },
            .allocator => {
                ref.allocator.analyte.undefined_safety = .{ .defined = {} };
            },
            .fnptr => {
                ref.fnptr.analyte.undefined_safety = .{ .defined = {} };
            },
            .region => |r| {
                // Region is a container type - don't set undefined state on it, just recurse
                retval_init_defined(refinements, r.to);
            },
            .recursive => {
                // Don't follow recursive references - they point back to an ancestor struct
                // that is already being processed. Following would cause infinite recursion.
            },
            .void => {}, // void return type is valid
            .unimplemented => @panic("retval_init_defined: unimplemented return type"),
            .noreturn => unreachable, // noreturn functions don't return, shouldn't reach here
        }
    }

    /// Initialize undefined state on a refinement created by typeToRefinement.
    /// Called when a Type is .undefined - marks scalars/pointers as undefined.
    /// This handles per-field undefined for struct fields like .{ .x = 42, .y = undefined }.
    pub fn init_from_type(ref: *Refinements.Refinement, is_undefined: bool, meta: Meta) void {
        if (!is_undefined) return;
        switch (ref.*) {
            .scalar => |*s| s.analyte.undefined_safety = .{ .undefined = .{ .meta = meta } },
            .pointer => |*p| p.analyte.undefined_safety = .{ .undefined = .{ .meta = meta } },
            .allocator => |*a| a.analyte.undefined_safety = .{ .undefined = .{ .meta = meta } },
            .fnptr => |*f| f.analyte.undefined_safety = .{ .undefined = .{ .meta = meta } },
            // Containers don't track undefined on themselves
            .optional, .errorunion, .@"struct", .@"union", .region, .recursive => {},
            .void, .noreturn, .unimplemented => {},
        }
    }

    /// Initialize the undefined state on a global variable refinement.
    /// The pointer to the global is always defined (it's a valid address).
    /// If is_undefined is true, marks the pointee as undefined; otherwise marks as defined.
    pub fn init_global(refinements: *Refinements, ptr_gid: Gid, pointee_gid: Gid, ctx: *Context, is_undefined: bool, is_null_opt: bool, loc: tag.GlobalLocation, field_info: ?tag.GlobalFieldInfo) void {
        _ = ctx;
        _ = is_null_opt; // Handled by null_safety.init_global
        _ = field_info; // Handled by fieldparentptr_safety.init_global

        // The pointer itself is always defined - it's a valid global address
        if (ptr_gid != 0) {
            const ptr_ref = refinements.at(ptr_gid);
            if (ptr_ref.* == .pointer) {
                ptr_ref.pointer.analyte.undefined_safety = .{ .defined = {} };
            }
        }

        // Handle the pointee's undefined state
        if (is_undefined) {
            // Construct Meta from the global's source location
            // Function name is empty for globals (they're not in a function)
            const meta = Meta{
                .function = "",
                .file = loc.file,
                .line = loc.line,
                .column = loc.column,
            };
            setUndefinedRecursive(refinements, pointee_gid, .{ .undefined = .{ .meta = meta } });
        } else {
            setDefinedRecursive(refinements, pointee_gid);
        }
    }

    // =========================================================================
    // Runtime Call Filter
    // =========================================================================

    /// Runtime call filter for undefined safety.
    /// Checks FQN patterns to intercept stdlib functions that need special handling.
    /// Returns true if intercepted (handled), false to continue with normal execution.
    /// Note: For allocator calls, memory_safety.call() returns true (intercepts) so we
    /// don't need to return true here - we just set the undefined state and return false.
    pub fn call(
        state: State,
        index: usize,
        return_type: tag.Type,
        args: []const tag.Src,
        fqn: []const u8,
    ) anyerror!bool {
        _ = return_type;

        if (gates.isAllocatorCreate(fqn)) {
            handleAllocatorCreate(state, index);
            return false; // memory_safety.call() intercepts
        }

        if (gates.isAllocatorAlloc(fqn)) {
            handleAllocatorAlloc(state, index);
            return false; // memory_safety.call() intercepts
        }

        // POSIX fd functions - check for undefined fd arguments
        if (gates.isPosixClose(fqn)) {
            // close(fd) - arg[0] is fd
            try checkArgUndefined(state, args, 0);
            return false; // fd_safety.call() intercepts
        }

        if (gates.isPosixRead(fqn) or gates.isPosixPread(fqn)) {
            // read(fd, buf, ...) - arg[0] is fd
            try checkArgUndefined(state, args, 0);
            return false;
        }

        if (gates.isPosixWrite(fqn) or gates.isPosixPwrite(fqn)) {
            // write(fd, buf, ...) - arg[0] is fd
            try checkArgUndefined(state, args, 0);
            return false;
        }

        if (gates.isPosixDup(fqn)) {
            // dup(oldfd) - arg[0] is fd
            try checkArgUndefined(state, args, 0);
            return false; // fd_safety.call() intercepts
        }

        if (gates.isPosixDup2(fqn)) {
            // dup2(oldfd, newfd) - arg[0] and arg[1] are fds
            try checkArgUndefined(state, args, 0);
            try checkArgUndefined(state, args, 1);
            return false; // fd_safety.call() intercepts
        }

        // fd-opening functions - mark result as defined
        if (gates.isPosixOpen(fqn) or gates.isPosixOpenat(fqn) or
            gates.isPosixSocket(fqn) or gates.isPosixAccept(fqn) or
            gates.isPosixEpollCreate(fqn) or gates.isPosixPipe(fqn))
        {
            handleFdOpen(state, index);
            return false; // fd_safety.call() intercepts
        }

        // Formatter functions - check tuple args for undefined values
        if (gates.isFormatter(fqn)) {
            try handleFormatter(state, args);
            return true; // Intercept - stub is generated
        }

        return false;
    }

    /// Handle formatter functions - check all args recursively for undefined values
    fn handleFormatter(state: State, args: []const tag.Src) !void {
        for (args) |src| {
            const arg_gid: Refinements.Gid = switch (src) {
                // Inst args must have refinements
                .inst => |inst| state.results[inst].refinement.?,
                // Interned values are comptime constants - always defined
                .interned => continue,
                // Function pointers can't be undefined (they're addresses)
                .fnptr => continue,
            };
            try checkUndefinedRecursive(state, arg_gid);
        }
    }

    /// Recursively check a refinement for undefined values (for tuple/struct args)
    fn checkUndefinedRecursive(state: State, gid: Refinements.Gid) !void {
        const ref = state.refinements.at(gid);
        switch (ref.*) {
            .scalar => |s| {
                const undef = s.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(state.ctx),
                    .inconsistent => return undef.reportInconsistentBranches(state.ctx),
                    .defined => {},
                }
            },
            .pointer => |p| {
                const undef = p.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(state.ctx),
                    .inconsistent => return undef.reportInconsistentBranches(state.ctx),
                    .defined => {},
                }
            },
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkUndefinedRecursive(state, field_gid);
                }
            },
            .@"union" => |u| {
                for (u.fields) |field_gid_opt| {
                    if (field_gid_opt) |field_gid| {
                        try checkUndefinedRecursive(state, field_gid);
                    }
                }
            },
            .optional => |o| try checkUndefinedRecursive(state, o.to),
            .errorunion => |e| try checkUndefinedRecursive(state, e.to),
            .region => |r| try checkUndefinedRecursive(state, r.to),
            .allocator, .fnptr, .void, .noreturn, .unimplemented, .recursive => {},
        }
    }

    /// Check if argument at given index is undefined
    fn checkArgUndefined(state: State, args: []const tag.Src, arg_index: usize) !void {
        if (arg_index >= args.len) return;

        const src = args[arg_index];
        const arg_gid: Refinements.Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned, .fnptr => return, // Interned values are always defined
        };

        const arg_ref = state.refinements.at(arg_gid);
        switch (arg_ref.*) {
            .scalar => |s| {
                const undef = s.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(state.ctx),
                    .inconsistent => return undef.reportInconsistentBranches(state.ctx),
                    .defined => {},
                }
            },
            .pointer => |p| {
                const undef = p.analyte.undefined_safety orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(state.ctx),
                    .inconsistent => return undef.reportInconsistentBranches(state.ctx),
                    .defined => {},
                }
            },
            else => {},
        }
    }

    /// Handle fd-opening functions - mark result as defined
    fn handleFdOpen(state: State, index: usize) void {
        const result_idx = state.results[index].refinement orelse return;
        // fd functions return errorunion -> scalar (fd_t)
        // Mark the entire result tree as defined
        setDefinedRecursive(state.refinements, result_idx);
    }

    /// Handle mem.Allocator.create - mark pointee as undefined.
    /// Result structure: errorunion -> ptr -> pointee
    fn handleAllocatorCreate(state: State, index: usize) void {
        const results = state.results;
        const refinements = state.refinements;

        const eu_idx = results[index].refinement orelse return;
        const eu_ref = refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return;

        const ptr_idx = eu_ref.errorunion.to;
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        // The pointer itself is defined (it exists)
        ptr_ref.pointer.analyte.undefined_safety = .{ .defined = {} };

        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = ptr_ref.pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }

    /// Handle mem.Allocator.alloc/dupe/dupeZ - mark elements as undefined.
    /// Result structure: errorunion -> pointer -> region -> element
    fn handleAllocatorAlloc(state: State, index: usize) void {
        const results = state.results;
        const refinements = state.refinements;

        const eu_idx = results[index].refinement orelse return;
        const eu_ref = refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return;

        const ptr_idx = eu_ref.errorunion.to;
        const ptr_ref = refinements.at(ptr_idx);
        if (ptr_ref.* != .pointer) return;

        // The pointer itself is defined (the slice exists)
        ptr_ref.pointer.analyte.undefined_safety = .{ .defined = {} };

        // The region is a container type - don't set undefined state on it
        const region_idx = ptr_ref.pointer.to;
        const region_ref = refinements.at(region_idx);
        if (region_ref.* != .region) return;

        // The elements start as undefined (must be set before use)
        const element_idx = region_ref.region.to;
        setUndefinedRecursive(refinements, element_idx, .{ .undefined = .{ .meta = state.ctx.meta } });
    }
};

const debug = @import("builtin").mode == .Debug;

/// Validate that a refinement conforms to undefined tracking rules:
/// - For .optional, .errorunion and .struct, the top-level analyte.undefined must be null
/// - For .scalar and .pointer, the analyte.undefined must be set (not null)
pub fn testValid(refinement: Refinements.Refinement, idx: usize) void {
    if (!debug) return;
    switch (refinement) {
        .scalar => |s| {
            if (s.analyte.undefined_safety == null) {
                std.debug.panic("undefined state must be set on scalars (refinement idx {})", .{idx});
            }
        },
        .pointer => |p| {
            if (p.analyte.undefined_safety == null) {
                std.debug.panic("undefined state must be set on pointers", .{});
            }
        },
        .fnptr => |f| {
            // Function pointers are values that can be undefined (like scalars/pointers)
            if (f.analyte.undefined_safety == null) {
                std.debug.panic("undefined state must be set on fnptrs", .{});
            }
        },
        inline .optional, .errorunion, .@"struct", .region => |data, t| {
            // Note: .@"union" is intentionally not included here - unions use analyte.undefined
            // for tracking state when activating inactive fields
            if (data.analyte.undefined_safety != null) {
                std.debug.panic("undefined state should not exist on container types, got {s} at idx {} (undefined_safety={any})", .{ @tagName(t), idx, data.analyte.undefined_safety });
            }
        },
        else => {},
    }
}

/// Helper to create a test context with specific meta values
fn initTestContext(allocator: std.mem.Allocator, discarding: *std.Io.Writer.Discarding, file: []const u8, line: u32, column: ?u32) Context {
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.file = file;
    ctx.meta.line = line;
    ctx.meta.column = column;
    ctx.meta.function = "test_func";
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

test "alloc creates pointer to typed pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Use Inst.apply which calls tag.Alloc.apply (creates pointer to typed pointee)
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // alloc creates pointer; pointee type is determined by .ty parameter
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(pointee_idx).*));
    refinements.testValid();
}

test "alloc_create creates errorunion -> pointer -> pointee" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First create an allocator refinement at inst 0 (memory_safety.alloc_create expects this)
    const alloc_ref = try refinements.appendEntity(.{ .allocator = .{ .type_id = 10 } });
    results[0].refinement = alloc_ref;

    // Use Inst.apply which calls tag.AllocCreate.apply (creates errorunion -> ptr -> pointee)
    try Inst.apply(state, 1, .{ .alloc_create = .{ .type_id = 10, .allocator_inst = 0, .ty = .{ .scalar = {} } } });

    // alloc_create creates errorunion -> pointer -> pointee (scalar in this case)
    const eu_idx = results[1].refinement.?;
    try std.testing.expectEqual(.errorunion, std.meta.activeTag(refinements.at(eu_idx).*));
    const ptr_idx = refinements.at(eu_idx).errorunion.to;
    try std.testing.expectEqual(.pointer, std.meta.activeTag(refinements.at(ptr_idx).*));
    const pointee_idx = refinements.at(ptr_idx).pointer.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(pointee_idx).*));
}

test "store with .undefined type wrapper sets undefined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store with .undefined wrapper
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .undefined = &.{ .scalar = {} } } } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(undef));
}

test "store with defined value sets defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // First alloc at instruction 1, then store a defined value
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .scalar = {} } } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(undef));
}

test "store with .null to optional sets inner to defined" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Alloc at instruction 1 with optional type, then store null
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .optional = &.{ .scalar = {} } } } });
    // Store null to the optional - inner should be defined (null is a valid defined value)
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .interned = .{ .ip_idx = 0, .ty = .{ .@"null" = &.{ .scalar = {} } } } } } });

    // Check the pointee is an optional
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.optional, std.meta.activeTag(refinements.at(pointee_idx).*));

    // Check the optional's inner value is a defined scalar
    const inner_idx = refinements.at(pointee_idx).optional.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(inner_idx).*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(inner_idx).scalar.analyte.undefined_safety.?));
}

test "store struct copies undefined_safety from source fields to destination fields" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 5;
    const state = testState(&ctx, &results, &refinements);

    // Instruction 1: alloc a pointer to struct with 2 scalar fields
    // This creates destination struct with undefined fields
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
    } } } });

    // Check that destination fields start as undefined
    const ptr_gid = results[1].refinement.?;
    const dst_struct_gid = refinements.at(ptr_gid).pointer.to;
    const dst_field0_gid = refinements.at(dst_struct_gid).@"struct".fields[0];
    const dst_field1_gid = refinements.at(dst_struct_gid).@"struct".fields[1];
    try std.testing.expectEqual(.undefined, std.meta.activeTag(refinements.at(dst_field0_gid).scalar.analyte.undefined_safety.?));
    try std.testing.expectEqual(.undefined, std.meta.activeTag(refinements.at(dst_field1_gid).scalar.analyte.undefined_safety.?));

    // Create source scalars with defined state
    const src_scalar0_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} }, .memory_safety = .{ .unset = {} } },
    } });
    results[2].refinement = src_scalar0_gid;
    const src_scalar1_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .defined = {} }, .memory_safety = .{ .unset = {} } },
    } });
    results[3].refinement = src_scalar1_gid;

    // Create source struct using aggregate_init (this properly allocates fields)
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 2 }, .{ .inst = 3 } };
    try Inst.apply(state, 4, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Store the source struct to the destination (through the pointer)
    try Inst.apply(state, 0, .{ .store = .{ .ptr = .{ .inst = 1 }, .src = .{ .inst = 4 } } });

    // Check that destination fields are now defined (copied from source)
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(dst_field0_gid).scalar.analyte.undefined_safety.?));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(dst_field1_gid).scalar.analyte.undefined_safety.?));
}

test "load from undefined inst returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create pointer -> undefined scalar
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .{ .undefined = .{ .meta = .{
            .function = "test_func",
            .file = "test.zig",
            .line = 1,
        } } } },
    } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .to = pointee_idx } });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_gid = 0 };
    try std.testing.expectError(
        error.UseBeforeAssign,
        UndefinedSafety.load(state, 0, .{ .ptr = .{ .inst = 1 } }),
    );
}

test "load from defined inst does not return error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create pointer -> defined scalar
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined_safety = .{ .defined = {} } } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .to = pointee_idx } });

    // Set up result for the load instruction (index 0)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{} });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_gid = 0 };
    try UndefinedSafety.load(state, 0, .{ .ptr = .{ .inst = 1 } });
}

test "load from inst without undefined tracking does not return error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;

    // Create pointer -> scalar with no undefined tracking (undefined = null)
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined_safety = null } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .to = pointee_idx } });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_gid = 0 };
    try UndefinedSafety.load(state, 0, .{ .ptr = .{ .inst = 1 } });
}

fn testGetName(id: u32) []const u8 {
    return switch (id) {
        1 => "my_var",
        else => "unknown",
    };
}

test "reportUseBeforeAssign with name_when_set returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    ctx.getName = &testGetName;
    defer ctx.deinit();

    const undef = UndefinedSafety{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
        .name_when_set = "my_var",
    } };

    try std.testing.expectError(error.UseBeforeAssign, undef.reportUseBeforeAssign(&ctx));
}

test "reportUseBeforeAssign without name_when_set returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    defer ctx.deinit();

    const undef = UndefinedSafety{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
    } };

    try std.testing.expectError(error.UseBeforeAssign, undef.reportUseBeforeAssign(&ctx));
}

test "aggregate_init incorporates undefined state from source elements" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = initTestContext(allocator, &discarding, "test.zig", 10, 5);
    defer ctx.deinit();

    var refinements = Refinements.init(allocator);
    defer refinements.deinit();

    var results = [_]Inst{.{}} ** 3;
    const state = testState(&ctx, &results, &refinements);

    // Manually create scalar refinements with defined and undefined states
    // inst 0: defined scalar
    const defined_scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{ .undefined_safety = .defined, .memory_safety = .{ .unset = {} } },
    } });
    results[0].refinement = defined_scalar_gid;

    // inst 1: undefined scalar
    const undefined_scalar_gid = try refinements.appendEntity(.{ .scalar = .{
        .analyte = .{
            .undefined_safety = .{ .undefined = .{
                .meta = .{ .function = "test", .file = "test.zig", .line = 5, .column = 10 },
            } },
            .memory_safety = .{ .unset = {} },
        },
    } });
    results[1].refinement = undefined_scalar_gid;

    // Create struct with two scalar fields using aggregate_init
    // Field 0 should be defined (from inst 0), field 1 should be undefined (from inst 1)
    const struct_type = tag.Type{ .@"struct" = &.{
        .type_id = 100,
        .fields = &.{ .{ .scalar = {} }, .{ .scalar = {} } },
    } };
    const elements = &[_]tag.Src{ .{ .inst = 0 }, .{ .inst = 1 } };
    try Inst.apply(state, 2, .{ .aggregate_init = .{ .ty = struct_type, .elements = elements } });

    // Check the struct's fields
    const struct_gid = results[2].refinement.?;
    const struct_ref = refinements.at(struct_gid);
    try std.testing.expectEqual(.@"struct", std.meta.activeTag(struct_ref.*));

    const field0_gid = struct_ref.@"struct".fields[0];
    const field1_gid = struct_ref.@"struct".fields[1];

    // Field 0 should be defined (from defined source)
    const field0_undef = refinements.at(field0_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.defined, std.meta.activeTag(field0_undef));

    // Field 1 should be undefined (from undefined source)
    const field1_undef = refinements.at(field1_gid).scalar.analyte.undefined_safety.?;
    try std.testing.expectEqual(.undefined, std.meta.activeTag(field1_undef));

    refinements.testValid();
}
