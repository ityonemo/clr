const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const EIdx = Inst.EIdx;
const Meta = @import("../Meta.zig");
const tag = @import("../tag.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// NOTE: For .optional, .errorunion and .struct refinements, the top-level analyte.undefined should always be null.
// We don't track undefined state on the container itself - only on the payload/fields.

pub const Undefined = union(enum) {
    defined: void,
    undefined: struct {
        meta: Meta,
        name_when_set: ?[]const u8 = null, // Access path when set to undefined
    },
    inconsistent: struct {
        undefined_meta: Meta, // where undefined was set
        branch_meta: Meta, // where the conditional branch occurred
        name_when_set: ?[]const u8 = null, // Access path when set to undefined
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

    /// Ensure an entity and its children have undefined state set.
    /// Only sets state if currently null (newly created), does not overwrite existing state.
    fn ensureUndefinedStateSet(refinements: *Refinements, idx: EIdx) void {
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
            .void, .unimplemented, .noreturn, .retval_future => {},
            .region => @panic("ensureUndefinedStateSet: region not yet implemented"),
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

        // Follow pointer to get to union - Zig's safety checks will panic on wrong types
        const ptr_ref = results[params.ptr.?].refinement.?;
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
    pub const cmp_lte = markResultDefined;
    pub const ctz = markResultDefined;
    pub const sub = markResultDefined;
    pub const is_non_err = markResultDefined;
    pub const unwrap_errunion_err = markResultDefined;

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
    fn setDefinedRecursive(refinements: *Refinements, idx: EIdx) void {
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
            .void, .unimplemented, .noreturn, .retval_future => {},
            .region => @panic("setDefinedRecursive: region not yet implemented"),
        }
    }

    /// Recursively set name_when_set on undefined states that don't have a name yet.
    /// Called by DbgVarPtr to retroactively name undefined states that were created
    /// before the variable name was known (since dbg_var_ptr comes AFTER stores in AIR).
    pub fn setNameOnUndefined(refinements: *Refinements, idx: EIdx, name: []const u8) void {
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
        setNameOnUndefined(state.refinements, pointee_idx, params.name);
    }

    pub fn optional_payload(state: State, index: usize, params: tag.OptionalPayload) !void {
        _ = state;
        _ = index;
        // For .eidx: result shares source's entity, undefined state already correct
        if (params.src == .interned) {
            @panic("optional_payload: interned source unimplemented");
        }
    }

    /// For interned (compile-time constant) args, set everything to defined.
    /// For eidx args, the entity was copied with its existing state - no change needed.
    pub fn arg(state: State, index: usize, params: tag.Arg) !void {
        switch (params.value) {
            .interned => {
                // Compile-time constants are always defined
                const idx = state.results[index].refinement.?;
                setDefinedRecursive(state.refinements, idx);
            },
            .eidx => {
                // Runtime value - undefined state was already copied from caller
            },
            .other => {},
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
            .eidx => {}, // Source has existing refinement with undefined state
            .interned => {
                const block_idx = state.results[params.block].refinement orelse return;
                setDefinedRecursive(state.refinements, block_idx);
            },
            .other => @panic("br: unexpected .other source"),
        }
    }

    /// ret_safe with interned values creates new entities via typeToRefinement.
    /// Those entities need undefined state set - interned values are compile-time constants, so defined.
    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = index;
        // Only handle interned non-void returns - those are the ones that create new entities
        const caller_refinements = state.caller_refinements orelse return;
        switch (params.src) {
            .interned => |ty| {
                if (ty.ty != .void) {
                    // Interned values are compile-time constants, so defined
                    setDefinedRecursive(caller_refinements, state.return_eidx);
                }
            },
            .eidx => {}, // Already has undefined state from callee
            .other => {},
        }
    }

    /// Recursively set undefined state on a refinement and its children.
    /// Only called when storing an undefined value (src type is .undefined).
    fn setUndefinedRecursive(refinements: *Refinements, idx: EIdx, undef_state: Undefined) void {
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
            .void, .unimplemented, .noreturn, .retval_future => {},
            .region => @panic("setUndefinedRecursive: region not yet implemented"),
        }
    }

    /// Apply defined/undefined state from an interned type to a refinement.
    /// Handles field-level undefined for structs where some fields may be undefined.
    fn applyInternedType(refinements: *Refinements, idx: EIdx, ty: tag.Type, ctx: *Context) void {
        switch (ty.ty) {
            .undefined => {
                // Mark as undefined - don't recurse further, undefined is terminal
                const undef_state: Undefined = .{ .undefined = .{ .meta = ctx.meta } };
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
            .void => {},
            .region => @panic("applyInternedType: region not yet implemented"),
        }
    }

    pub fn store(state: State, index: usize, params: tag.Store) !void {
        _ = index;
        const results = state.results;
        const refinements = state.refinements;

        const ptr = params.ptr orelse @panic("store: ptr is null (interned/global) - not yet supported");
        // Follow pointer to get to pointee (local only - propagation happens on function close)
        const ptr_idx = results[ptr].refinement orelse @panic("store: ptr inst has no refinement");
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = refinements.at(ptr_idx).pointer.to;

        // Check if source is an undefined type (interned with .undefined wrapper)
        const is_undef = switch (params.src) {
            .interned => |ty| ty.ty == .undefined,
            else => false,
        };

        if (is_undef) {
            // Undefined stores: mark pointee and all children as undefined (recursive)
            // Capture name from the destination pointer instruction
            const undef_state: Undefined = .{ .undefined = .{ .meta = state.ctx.meta, .name_when_set = results[ptr].name } };
            setUndefinedRecursive(refinements, pointee_idx, undef_state);
        } else {
            // Defined stores: update the pointee based on source type
            switch (params.src) {
                .eidx => |src_idx| {
                    // Runtime source - connect pointee to source's entity
                    const src_ref = results[src_idx].refinement orelse {
                        // Source has no refinement - just mark as defined
                        switch (refinements.at(pointee_idx).*) {
                            .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                            .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                            else => {},
                        }
                        return;
                    };
                    // When storing a pointer value, update the destination's `to` field
                    // so loads through the destination will reach the same target
                    switch (refinements.at(pointee_idx).*) {
                        .scalar => |*s| s.analyte.undefined = .{ .defined = {} },
                        .pointer => |*p| {
                            // If source is also a pointer, copy its structure
                            if (refinements.at(src_ref).* == .pointer) {
                                const src_ptr = refinements.at(src_ref).pointer;
                                p.to = src_ptr.to;
                                // Copy analyte but override undefined to defined
                                p.analyte = src_ptr.analyte;
                                p.analyte.undefined = .{ .defined = {} };
                            } else {
                                p.analyte.undefined = .{ .defined = {} };
                            }
                        },
                        .optional => |o| {
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
                        .void, .unimplemented, .noreturn, .retval_future => {},
                        .region => @panic("store: region not yet implemented"),
                    }
                },
                .interned => |ty| {
                    // Compile-time source - apply defined/undefined based on type
                    applyInternedType(refinements, pointee_idx, ty, state.ctx);
                },
                .other => {
                    // Other sources (globals) - just mark as defined
                    setDefinedRecursive(refinements, pointee_idx);
                },
            }
        }
    }

    pub fn load(state: State, index: usize, params: tag.Load) !void {
        const results = state.results;
        const refinements = state.refinements;
        const ctx = state.ctx;
        const ptr = params.ptr orelse {
            // Interned/global pointer - tag handler created new entity, set undefined state
            const result_idx = results[index].refinement.?;
            ensureUndefinedStateSet(refinements, result_idx);
            return;
        };
        const ptr_idx = results[ptr].refinement orelse {
            // No refinement on pointer - tag handler created new entity, set undefined state
            const result_idx = results[index].refinement.?;
            ensureUndefinedStateSet(refinements, result_idx);
            return;
        };
        // Follow pointer to get to pointee
        const pointee_idx = switch (refinements.at(ptr_idx).*) {
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
            .region => @panic("load: region - undefined tracking not yet implemented"),
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

    fn checkFieldUndefined(refinements: *Refinements, field_idx: Refinements.EIdx, ctx: *Context) !void {
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

    /// Merge undefined states from two branches after a conditional.
    /// Walks the refinement tree and reports if branches have inconsistent initialization.
    pub fn merge(
        ctx: *Context,
        comptime merge_tag: anytype,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) !void {
        _ = merge_tag; // Available for context-specific merge behavior
        mergeRefinement(ctx, orig, true_branch, false_branch);
    }

    fn mergeRefinement(
        ctx: *Context,
        orig: struct { *Refinements, EIdx },
        true_branch: struct { *Refinements, EIdx },
        false_branch: struct { *Refinements, EIdx },
    ) void {
        const orig_ref = orig[0].at(orig[1]);
        const true_ref = true_branch[0].at(true_branch[1]);
        const false_ref = false_branch[0].at(false_branch[1]);

        switch (orig_ref.*) {
            .scalar => |*s| {
                const true_undef = true_ref.scalar.analyte.undefined orelse return;
                const false_undef = false_ref.scalar.analyte.undefined orelse return;
                s.analyte.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
            },
            .pointer => |*p| {
                // Merge analyte on the pointer itself
                if (true_ref.pointer.analyte.undefined) |true_undef| {
                    if (false_ref.pointer.analyte.undefined) |false_undef| {
                        p.analyte.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
                    }
                }
                // Recursively merge pointee
                mergeRefinement(
                    ctx,
                    .{ orig[0], p.to },
                    .{ true_branch[0], true_ref.pointer.to },
                    .{ false_branch[0], false_ref.pointer.to },
                );
            },
            .optional => |*o| {
                // Optional containers don't track undefined - just recurse into payload
                mergeRefinement(
                    ctx,
                    .{ orig[0], o.to },
                    .{ true_branch[0], true_ref.optional.to },
                    .{ false_branch[0], false_ref.optional.to },
                );
            },
            .errorunion => |*e| {
                // Error union containers don't track undefined - just recurse into payload
                mergeRefinement(
                    ctx,
                    .{ orig[0], e.to },
                    .{ true_branch[0], true_ref.errorunion.to },
                    .{ false_branch[0], false_ref.errorunion.to },
                );
            },
            .region => @panic("regions not implemented yet"),
            // Types without undefined tracking - no merge needed
            .void, .unimplemented, .noreturn => {},
            .@"struct" => |*s| {
                // Struct containers don't track undefined - just recurse into fields
                for (s.fields, 0..) |field_idx, i| {
                    mergeRefinement(
                        ctx,
                        .{ orig[0], field_idx },
                        .{ true_branch[0], true_ref.@"struct".fields[i] },
                        .{ false_branch[0], false_ref.@"struct".fields[i] },
                    );
                }
            },
            .@"union" => |*u| {
                // Union "dumb merge": union all active fields from both branches
                // Only recurse into fields that are active in all three (orig, true, false)
                const true_union = true_ref.@"union";
                const false_union = false_ref.@"union";
                for (u.fields, 0..) |orig_field_opt, i| {
                    const orig_field = orig_field_opt orelse continue;
                    const true_field = true_union.fields[i] orelse continue;
                    const false_field = false_union.fields[i] orelse continue;
                    mergeRefinement(
                        ctx,
                        .{ orig[0], orig_field },
                        .{ true_branch[0], true_field },
                        .{ false_branch[0], false_field },
                    );
                }
            },
            .retval_future => @panic("mergeRefinement: cannot merge .retval_future"),
        }
    }

    fn mergeUndefinedStates(ctx: *Context, true_undef: Undefined, false_undef: Undefined) Undefined {
        // Handle .inconsistent propagation first - once inconsistent, stays inconsistent
        if (true_undef == .inconsistent) return true_undef;
        if (false_undef == .inconsistent) return false_undef;

        const true_is_defined = true_undef == .defined;
        const false_is_defined = false_undef == .defined;

        // Both agree
        if (true_is_defined and false_is_defined) return .{ .defined = {} };
        if (!true_is_defined and !false_is_defined) return true_undef; // both undefined, use first's meta

        // Inconsistent: one defined, one undefined -> return .inconsistent
        const undef_state = if (!true_is_defined) true_undef.undefined else false_undef.undefined;
        return .{ .inconsistent = .{
            .undefined_meta = undef_state.meta,
            .branch_meta = ctx.meta,
            .name_when_set = undef_state.name_when_set,
        } };
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
        inline .optional, .errorunion, .@"struct" => |data, t| {
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
        .return_eidx = 0,
        .caller_refinements = null,
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });

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
    try Inst.apply(state, 1, .{ .alloc_create = .{ .type_id = 10, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });

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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = 0, .ty = .{ .undefined = &.{ .id = 0, .ty = .{ .scalar = {} } } } } } } });

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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .scalar = {} } } } });
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = 0, .ty = .{ .scalar = {} } } } } });

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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .id = 0, .ty = .{ .optional = &.{ .id = 0, .ty = .{ .scalar = {} } } } } } });
    // Store null to the optional - inner should be defined (null is a valid defined value)
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .id = 0, .ty = .{ .null = &.{ .id = 0, .ty = .{ .scalar = {} } } } } } } });

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
        .type_id = 0,
    } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = pointee_idx } });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_eidx = 0, .caller_refinements = null };
    try std.testing.expectError(
        error.UseBeforeAssign,
        Undefined.load(state, 0, .{ .ptr = 1, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } }),
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = .{ .defined = {} } }, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = pointee_idx } });

    // Set up result for the load instruction (index 0)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{ .analyte = .{}, .type_id = 0 } });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_eidx = 0, .caller_refinements = null };
    try Undefined.load(state, 0, .{ .ptr = 1, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } });
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .analyte = .{ .undefined = null }, .type_id = 0 } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .type_id = 0, .to = pointee_idx } });

    const state = State{ .ctx = &ctx, .results = &results, .refinements = &refinements, .return_eidx = 0, .caller_refinements = null };
    try Undefined.load(state, 0, .{ .ptr = 1, .ty = .{ .id = 0, .ty = .{ .scalar = {} } } });
}

test "reportUseBeforeAssign with name_when_set returns error" {
    const allocator = std.testing.allocator;

    var buf: [4096]u8 = undefined;
    var discarding = std.Io.Writer.Discarding.init(&buf);
    var ctx = Context.init(allocator, &discarding.writer);
    ctx.meta.function = "test_func";
    defer ctx.deinit();

    const undef = Undefined{ .undefined = .{
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

    const undef = Undefined{ .undefined = .{
        .meta = .{
            .function = "test_func",
            .file = "file.zig",
            .line = 42,
            .column = 8,
        },
    } };

    try std.testing.expectError(error.UseBeforeAssign, undef.reportUseBeforeAssign(&ctx));
}
