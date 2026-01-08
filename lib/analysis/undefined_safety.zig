const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Gid = Refinements.Gid;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
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
            .scalar => |s| if (s.analyte.undefined) |u| u == .undefined else false,
            .pointer => |p| if (p.analyte.undefined) |u| u == .undefined else false,
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
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
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
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
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
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
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
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
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
        ptr.analyte.undefined = .{ .defined = {} };
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
        ptr.analyte.undefined = .{ .defined = {} };
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
        ptr.analyte.undefined = .{ .defined = {} };
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
        ptr.analyte.undefined = .{ .defined = {} };
        // The payload's undefined state is already set from the error union
    }

    pub fn slice_ptr(state: State, index: usize, params: tag.SlicePtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists and points to a valid region)
        const ptr_idx = results[index].refinement.?;
        const ptr = &refinements.at(ptr_idx).pointer;
        ptr.analyte.undefined = .{ .defined = {} };
        // The region's undefined state is already set (from alloc)
    }

    /// Ensure an entity and its children have undefined state set.
    /// Only sets state if currently null (newly created), does not overwrite existing state.
    fn ensureUndefinedStateSet(refinements: *Refinements, idx: Gid) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                if (s.analyte.undefined == null) {
                    s.analyte.undefined = .{ .defined = {} };
                }
            },
            .pointer => |*p| {
                if (p.analyte.undefined == null) {
                    p.analyte.undefined = .{ .defined = {} };
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
                if (a.analyte.undefined == null) {
                    a.analyte.undefined = .{ .defined = {} };
                }
            },
            .void, .unimplemented, .noreturn => {},
            .region => |r| {
                // Don't set analyte.undefined on region container - only the uniform element carries undefined state
                ensureUndefinedStateSet(refinements, r.to);
            },
        }
    }

    pub fn ret_ptr(state: State, index: usize, params: tag.RetPtr) !void {
        _ = params;
        const results = state.results;
        const refinements = state.refinements;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
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
            .int_var => |nav_idx| refinements.getGlobal(nav_idx).?,
            .int_const => return, // comptime constant - no undefined tracking
        };
        const container_idx = refinements.at(ptr_ref).pointer.to;
        const u = &refinements.at(container_idx).@"union";

        // When a tag is set, the union container is being initialized - mark as defined
        u.analyte.undefined = .{ .defined = {} };

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
            refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
            return;
        };

        const union_ref = results[operand].refinement orelse {
            // No refinement on operand - assume defined
            const result_idx = results[index].refinement.?;
            refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
            return;
        };

        // Check if the union is defined
        const union_refinement = refinements.at(union_ref);
        if (union_refinement.* != .@"union") {
            std.debug.panic("get_union_tag: expected union, got {s}", .{@tagName(union_refinement.*)});
        }

        const union_undefined = union_refinement.@"union".analyte.undefined;
        const result_idx = results[index].refinement.?;

        if (union_undefined) |undef_state| {
            // Propagate undefined state from union to tag
            refinements.at(result_idx).scalar.analyte.undefined = undef_state;
        } else {
            // Union has no undefined tracking - assume defined
            refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
        }
    }

    // Simple operations produce defined scalar results
    pub const bit_and = markResultDefined;
    pub const cmp_eq = markResultDefined;
    pub const cmp_gt = markResultDefined;
    pub const cmp_lt = markResultDefined;
    pub const cmp_lte = markResultDefined;
    pub const ctz = markResultDefined;
    pub const slice_len = markResultDefined;
    pub const sub = markResultDefined;
    pub const is_non_err = markResultDefined;
    pub const unwrap_errunion_err = markResultDefined;
    pub const alloc_resize = markResultDefined; // resize returns a defined bool

    // Null checks produce defined boolean results
    pub const is_non_null = markResultDefined;
    pub const is_null = markResultDefined;

    // Overflow operations produce a struct with defined fields
    pub fn add_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        markStructFieldsDefined(state.results, index, state.refinements);
    }

    pub fn sub_with_overflow(state: State, index: usize, params: anytype) !void {
        _ = params;
        markStructFieldsDefined(state.results, index, state.refinements);
    }

    fn markResultDefined(state: State, index: usize, params: anytype) !void {
        _ = params;
        const result_idx = state.results[index].refinement.?;
        state.refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
    }

    fn markStructFieldsDefined(results: []Inst, index: usize, refinements: *Refinements) void {
        const result_idx = results[index].refinement.?;
        const s = refinements.at(result_idx).@"struct";
        for (s.fields) |field_idx| {
            refinements.at(field_idx).scalar.analyte.undefined = .{ .defined = {} };
        }
    }

    /// Helper to recursively set all scalars/pointers in a refinement tree to defined.
    fn setDefinedRecursive(refinements: *Refinements, idx: Gid) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
            .pointer => |*p| {
                p.analyte.undefined = .{ .defined = {} };
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
                u.analyte.undefined = .{ .defined = {} };
                // Also recurse into active fields (non-null)
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setDefinedRecursive(refinements, field_idx);
                    }
                }
            },
            .allocator => |*a| a.analyte.undefined = .{ .defined = {} },
            .void, .unimplemented, .noreturn => {},
            .region => |r| setDefinedRecursive(refinements, r.to),
        }
    }

    /// Recursively set name_when_set on undefined states that don't have a name yet.
    /// Called by DbgVarPtr to retroactively name undefined states that were created
    /// before the variable name was known (since dbg_var_ptr comes AFTER stores in AIR).
    pub fn setNameOnUndefined(refinements: *Refinements, idx: Gid, name: ?[]const u8) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| {
                if (s.analyte.undefined) |*undef| {
                    if (undef.* == .undefined and undef.undefined.name_when_set == null) {
                        undef.undefined.name_when_set = name;
                    }
                }
            },
            .pointer => |*p| {
                if (p.analyte.undefined) |*undef| {
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

    pub fn optional_payload(state: State, index: usize, params: tag.OptionalPayload) !void {
        _ = state;
        _ = index;
        // For .inst: result shares source's entity, undefined state already correct
        if (params.src == .int_const) {
            @panic("optional_payload: int_const source unimplemented");
        }
    }

    /// For int_const (compile-time constant) args, set everything to defined.
    /// For inst/int_var args, the entity was copied with its existing state - no change needed.
    pub fn arg(state: State, index: usize, params: tag.Arg) !void {
        switch (params.value) {
            .int_const => {
                // Compile-time constants are always defined
                const idx = state.results[index].refinement.?;
                setDefinedRecursive(state.refinements, idx);
            },
            .inst => {
                // Runtime value - undefined state was already copied from caller
            },
            .int_var => {
                // Interned var - undefined state was set during Refinements.initWithGlobals
            },
        }
    }

    /// br sets refinement on its target block, not on itself.
    /// When it creates scalars for the block, we need to set their undefined state.
    pub fn br(state: State, index: usize, params: tag.Br) !void {
        _ = index;
        // br sets refinement on self.block, not on its own index
        // When source is an eidx with existing refinement, undefined state is already set.
        // When br creates a new scalar (interned source), we need to set undefined.
        switch (params.src) {
            .inst, .int_var => {}, // Source has existing refinement with undefined state
            .int_const => {
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
            .int_const => |ty| {
                if (ty.ty != .void) {
                    // Interned values are compile-time constants, so defined
                    // With global refinements, return_gid points to slot in state.refinements
                    setDefinedRecursive(state.refinements, state.return_gid);
                }
            },
            .inst, .int_var => {}, // Already has undefined state from callee or initWithGlobals
        }
    }

    /// Recursively set undefined state on a refinement and its children.
    /// Only called when storing an undefined value (src type is .undefined).
    fn setUndefinedRecursive(refinements: *Refinements, idx: Gid, undef_state: UndefinedSafety) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.analyte.undefined = undef_state,
            .pointer => |*p| {
                p.analyte.undefined = undef_state;
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
                u.analyte.undefined = undef_state;
                // Also set on active fields
                for (u.fields) |field_idx_opt| {
                    if (field_idx_opt) |field_idx| {
                        setUndefinedRecursive(refinements, field_idx, undef_state);
                    }
                }
            },
            .allocator => |*a| a.analyte.undefined = undef_state,
            .void, .unimplemented, .noreturn => {},
            .region => |r| {
                // Don't set analyte.undefined on region - only the uniform element carries undefined state
                setUndefinedRecursive(refinements, r.to, undef_state);
            },
        }
    }

    /// Apply defined/undefined state from an interned type to a refinement.
    /// Handles field-level undefined for structs where some fields may be undefined.
    fn applyInternedType(refinements: *Refinements, idx: Gid, ty: tag.Type, ctx: *Context) void {
        switch (ty.ty) {
            .undefined => {
                // Mark as undefined - don't recurse further, undefined is terminal
                const undef_state: UndefinedSafety = .{ .undefined = .{ .meta = ctx.meta } };
                setUndefinedRecursive(refinements, idx, undef_state);
            },
            .@"struct" => |field_types| {
                // Apply field-level undefined state
                switch (refinements.at(idx).*) {
                    .@"struct" => |s| {
                        for (field_types, 0..) |field_type, i| {
                            if (i < s.fields.len) {
                                applyInternedType(refinements, s.fields[i], field_type, ctx);
                            }
                        }
                    },
                    else => {
                        // Non-struct refinement - just mark as defined
                        setDefinedRecursive(refinements, idx);
                    },
                }
            },
            .scalar => {
                switch (refinements.at(idx).*) {
                    .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                    .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                    else => {},
                }
            },
            .pointer => |inner| {
                switch (refinements.at(idx).*) {
                    .pointer => |*p| {
                        p.analyte.undefined = .{ .defined = {} };
                        applyInternedType(refinements, p.to, inner.*, ctx);
                    },
                    else => setDefinedRecursive(refinements, idx),
                }
            },
            .optional => |inner| {
                switch (refinements.at(idx).*) {
                    .optional => |o| applyInternedType(refinements, o.to, inner.*, ctx),
                    else => setDefinedRecursive(refinements, idx),
                }
            },
            .errorunion => |inner| {
                switch (refinements.at(idx).*) {
                    .errorunion => |e| applyInternedType(refinements, e.to, inner.*, ctx),
                    else => setDefinedRecursive(refinements, idx),
                }
            },
            .null => {
                // Null value - mark inner as defined (it's explicitly null, not undefined)
                switch (refinements.at(idx).*) {
                    .optional => |o| setDefinedRecursive(refinements, o.to),
                    else => setDefinedRecursive(refinements, idx),
                }
            },
            .@"union" => |field_types| {
                // Apply field-level undefined state for unions
                switch (refinements.at(idx).*) {
                    .@"union" => |*u| {
                        // Mark union itself as defined (since we're storing a value, not undefined)
                        u.analyte.undefined = .{ .defined = {} };
                        for (field_types, 0..) |field_type, i| {
                            if (i < u.fields.len) {
                                if (u.fields[i]) |field_idx| {
                                    applyInternedType(refinements, field_idx, field_type, ctx);
                                }
                            }
                        }
                    },
                    else => {
                        // Non-union refinement - just mark as defined
                        setDefinedRecursive(refinements, idx);
                    },
                }
            },
            .allocator => {
                // Allocator value - mark as defined
                switch (refinements.at(idx).*) {
                    .allocator => |*a| a.analyte.undefined = .{ .defined = {} },
                    else => setDefinedRecursive(refinements, idx),
                }
            },
            .void => {},
            .region => |inner| {
                switch (refinements.at(idx).*) {
                    .region => |r| applyInternedType(refinements, r.to, inner.*, ctx),
                    else => setDefinedRecursive(refinements, idx),
                }
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
            .int_var => |nav_idx| refinements.getGlobal(nav_idx) orelse @panic("store: global not found"),
            .int_const => @panic("store: storing through constant pointer not supported"),
        };
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = refinements.at(ptr_gid).pointer.to;

        // Check if source is an undefined type (int_const with .undefined wrapper)
        const is_undef = switch (params.src) {
            .int_const => |ty| ty.ty == .undefined,
            else => false,
        };

        if (is_undef) {
            // Undefined stores: mark pointee and all children as undefined (recursive)
            // Build full path name for the destination pointer
            const name_when_set: ?[]const u8 = switch (params.ptr) {
                .inst => |ptr| state.ctx.buildPathName(results, refinements, ptr),
                .int_var => null, // TODO: look up global name from nav_idx
                .int_const => null,
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
                            .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                            .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                            else => {},
                        }
                        return;
                    };
                    // Mark the pointee as defined based on its type
                    switch (refinements.at(pointee_idx).*) {
                        .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                        .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                        .optional => |o| {
                            // Mark the payload as defined
                            switch (refinements.at(o.to).*) {
                                .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                                .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                                else => {},
                            }
                        },
                        .errorunion => |e| {
                            switch (refinements.at(e.to).*) {
                                .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                                .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                                else => {},
                            }
                        },
                        .@"struct" => |*s| {
                            // When storing a struct value, mark all fields as defined
                            s.analyte.undefined = .{ .defined = {} };
                            for (s.fields) |field_idx| {
                                setDefinedRecursive(refinements, field_idx);
                            }
                        },
                        .@"union" => |*u| {
                            // When storing a union value, mark union and active fields as defined
                            u.analyte.undefined = .{ .defined = {} };
                            for (u.fields) |field_idx_opt| {
                                if (field_idx_opt) |field_idx| {
                                    setDefinedRecursive(refinements, field_idx);
                                }
                            }
                        },
                        .allocator => |*a| a.analyte.undefined = .{ .defined = {} },
                        .void, .unimplemented, .noreturn => {},
                        .region => |r| {
                            // When storing to a region, mark the uniform element as defined
                            setDefinedRecursive(refinements, r.to);
                        },
                    }
                },
                .int_const => |ty| {
                    // Compile-time source - apply defined/undefined based on type
                    applyInternedType(refinements, pointee_idx, ty, state.ctx);
                },
                .int_var => |nav_idx| {
                    // Global source - look up in global_map and handle like .inst
                    const global_gid = refinements.getGlobal(nav_idx) orelse {
                        // Global not found (shouldn't happen) - just mark as defined
                        setDefinedRecursive(refinements, pointee_idx);
                        return;
                    };
                    // When storing a pointer value, update the destination's `to` field
                    switch (refinements.at(pointee_idx).*) {
                        .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                        .pointer => |*p| {
                            // If global is also a pointer, update .to to share the target
                            if (refinements.at(global_gid).* == .pointer) {
                                const global_ptr = refinements.at(global_gid).pointer;
                                p.to = global_ptr.to;
                                p.analyte.undefined = .{ .defined = {} };
                            } else {
                                p.analyte.undefined = .{ .defined = {} };
                            }
                        },
                        else => setDefinedRecursive(refinements, pointee_idx),
                    }
                },
            }
        }
    }

    pub fn load(state: State, index: usize, params: tag.Load) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;

        // Get pointer GID based on source type
        const ptr_idx: ?Gid = switch (params.ptr) {
            .inst => |ptr| results[ptr].refinement,
            .int_var => |nav_idx| refinements.getGlobal(nav_idx),
            .int_const => {
                // Interned constant - tag handler created new entity, set undefined state
                const result_idx = results[index].refinement.?;
                ensureUndefinedStateSet(refinements, result_idx);
                return;
            },
        };
        const effective_ptr_idx = ptr_idx orelse {
            // No refinement on pointer - tag handler created new entity, set undefined state
            const result_idx = results[index].refinement.?;
            ensureUndefinedStateSet(refinements, result_idx);
            return;
        };
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(effective_ptr_idx).*) {
            .pointer => |p| p.to,
            else => {
                // Not a pointer - tag handler created new entity, set undefined state
                const result_idx = results[index].refinement.?;
                ensureUndefinedStateSet(refinements, result_idx);
                return;
            },
        };
        switch (refinements.at(pointee_idx).*) {
            .scalar => |s| {
                // undefined is null when value wasn't allocated through our tracked mechanisms
                // (e.g., external data, FFI). No undefined tracking available - skip.
                const undef = s.analyte.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.analyte.undefined = .{ .defined = {} };
                    },
                }
            },
            .pointer => |ind| {
                // undefined is null when pointer wasn't allocated through our tracked mechanisms.
                // No undefined tracking available - skip.
                const undef = ind.analyte.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // For compound types, we share the entity so state is already correct.
                        // Only propagate for fresh scalars.
                        const idx = results[index].refinement orelse return;
                        switch (refinements.at(idx).*) {
                            .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
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
                        const undef = s.analyte.undefined orelse return;
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
                        const undef = s.analyte.undefined orelse return;
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
                        const undef = s.analyte.undefined orelse return;
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    },
                    else => {}, // Other element types - recurse or skip
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
            refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
            return;
        };
        const container_ref = results[operand].refinement orelse {
            // No struct refinement - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
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
                    if (u.analyte.undefined) |undef| {
                        switch (undef) {
                            .undefined => return undef.reportUseBeforeAssign(ctx),
                            .inconsistent => return undef.reportInconsistentBranches(ctx),
                            .defined => {},
                        }
                    }
                    // The fresh entity created by typeToRefinement needs undefined state set
                    setDefinedRecursive(refinements, result_idx);
                }
            },
            else => {
                // Not a struct or union - result was created as fresh scalar, set to defined
                refinements.at(result_idx).scalar.analyte.undefined = .{ .defined = {} };
            },
        }
    }

    fn checkFieldUndefined(refinements: *Refinements, field_idx: Refinements.Gid, ctx: *Context) !void {
        switch (refinements.at(field_idx).*) {
            .scalar => |sc| {
                const undef = sc.analyte.undefined orelse @panic("struct_field_val: field scalar has no undefined state");
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {},
                }
            },
            .pointer => |ind| {
                const undef = ind.analyte.undefined orelse @panic("struct_field_val: field pointer has no undefined state");
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
                    s.analyte.undefined = merged;
                }
            },
            .pointer => |*p| {
                if (mergeUndefinedFromBranches(ctx, branches, branch_gids, getPointerUndefined)) |merged| {
                    p.analyte.undefined = merged;
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
        return ref.scalar.analyte.undefined;
    }

    /// Get undefined state from a pointer at given GID
    fn getPointerUndefined(branch: State, branch_gid: Gid) ?UndefinedSafety {
        const ref = branch.refinements.at(branch_gid);
        if (ref.* != .pointer) return null;
        return ref.pointer.analyte.undefined;
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
                ref.scalar.analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
            },
            .pointer => |p| {
                ref.pointer.analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
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
                ref.allocator.analyte.undefined = .{ .undefined = .{ .meta = ctx.meta } };
            },
            .region => |r| {
                // Region is a container type - don't set undefined state on it, just recurse
                retval_init(refinements, r.to, ctx);
            },
            .void => {}, // void return type is valid
            .noreturn, .unimplemented => unreachable,
        }
    }

    /// Initialize the undefined state on a return slot refinement as DEFINED.
    /// Used for interned comptime values (constants, null) that are not the .undefined type.
    pub fn retval_init_defined(refinements: *Refinements, gid: Gid) void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                ref.scalar.analyte.undefined = .{ .defined = {} };
            },
            .pointer => |p| {
                ref.pointer.analyte.undefined = .{ .defined = {} };
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
                ref.allocator.analyte.undefined = .{ .defined = {} };
            },
            .region => |r| {
                // Region is a container type - don't set undefined state on it, just recurse
                retval_init_defined(refinements, r.to);
            },
            .void => {}, // void return type is valid
            .noreturn, .unimplemented => unreachable,
        }
    }

    /// Initialize the undefined state on a global variable refinement.
    /// If is_undefined is true, marks as undefined; otherwise marks as defined.
    pub fn init_global(refinements: *Refinements, gid: Gid, ctx: *Context, is_undefined: bool, is_null_opt: bool, loc: tag.GlobalLocation) void {
        _ = ctx;
        _ = is_null_opt; // Handled by null_safety.init_global
        if (is_undefined) {
            // Construct Meta from the global's source location
            // Function name is empty for globals (they're not in a function)
            const meta = Meta{
                .function = "",
                .file = loc.file,
                .line = loc.line,
                .column = loc.column,
            };
            setUndefinedRecursive(refinements, gid, .{ .undefined = .{ .meta = meta } });
        } else {
            setDefinedRecursive(refinements, gid);
        }
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
            if (s.analyte.undefined == null) {
                std.debug.panic("undefined state must be set on scalars (refinement idx {})", .{idx});
            }
        },
        .pointer => |p| {
            if (p.analyte.undefined == null) {
                std.debug.panic("undefined state must be set on pointers", .{});
            }
        },
        inline .optional, .errorunion, .@"struct", .region => |data, t| {
            // Note: .@"union" is intentionally not included here - unions use analyte.undefined
            // for tracking state when activating inactive fields
            if (data.analyte.undefined != null) {
                std.debug.panic("undefined state should not exist on container types, got {s}", .{@tagName(t)});
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });

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

    // Use Inst.apply which calls tag.AllocCreate.apply (creates errorunion -> ptr -> pointee)
    try Inst.apply(state, 1, .{ .alloc_create = .{ .type_id = 10, .allocator_inst = null, .ty = .{ .ty = .{ .scalar = {} } } } });

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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .int_const = .{ .ty = .{ .undefined = &.{ .ty = .{ .scalar = {} } } } } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.analyte.undefined.?;
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .int_const = .{ .ty = .{ .scalar = {} } } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.analyte.undefined.?;
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .ty = .{ .optional = &.{ .ty = .{ .scalar = {} } } } } } });
    // Store null to the optional - inner should be defined (null is a valid defined value)
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = .{ .inst = 1 }, .src = .{ .int_const = .{ .ty = .{ .null = &.{ .ty = .{ .scalar = {} } } } } } } });

    // Check the pointee is an optional
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.optional, std.meta.activeTag(refinements.at(pointee_idx).*));

    // Check the optional's inner value is a defined scalar
    const inner_idx = refinements.at(pointee_idx).optional.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(inner_idx).*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(inner_idx).scalar.analyte.undefined.?));
}

// TODO: Interprocedural tests disabled during entity system refactoring.
// test "store_safe propagates defined through arg_ptr" { ... }

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
        .analyte = .{ .undefined = .{ .undefined = .{ .meta = .{
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = .{ .defined = {} } } } });
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = null } } });
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
