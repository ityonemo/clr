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

    pub fn alloc(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Alloc) !void {
        _ = params;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = ctx.meta } });
    }

    pub fn alloc_create(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.AllocCreate) !void {
        _ = params;
        // Result is errorunion -> ptr -> pointee
        const eu_idx = results[index].refinement.?;
        const ptr_idx = refinements.at(eu_idx).errorunion.to;
        // The pointer itself is defined (it exists)
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
        // The pointee starts as undefined (must be set by store before use)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = ctx.meta } });
    }

    pub fn struct_field_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StructFieldPtr) !void {
        _ = ctx;
        _ = params;
        // The pointer itself is defined (it exists and points to a valid field)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
    }

    pub fn ret_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.RetPtr) !void {
        _ = params;
        // The pointer itself is defined (it exists)
        const ptr_idx = results[index].refinement.?;
        refinements.at(ptr_idx).pointer.analyte.undefined = .{ .defined = {} };
        // The pointee starts as undefined (must be set before ret_load)
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setUndefinedRecursive(refinements, pointee_idx, .{ .undefined = .{ .meta = ctx.meta } });
    }

    /// Helper to recursively set all scalars/pointers in a refinement tree to defined.
    fn setDefinedRecursive(refinements: *Refinements, idx: EIdx) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.undefined = .{ .defined = {} },
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
                if (s.undefined) |*undef| {
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
    pub fn dbg_var_ptr(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.DbgVarPtr) !void {
        _ = index;
        _ = ctx;
        const inst = params.ptr orelse return;
        const ptr_idx = results[inst].refinement orelse return;
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        setNameOnUndefined(refinements, pointee_idx, params.name);
    }

    // Handlers for operations that produce defined scalar results
    pub fn cmp_eq(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Simple(.cmp_eq)) !void {
        _ = ctx;
        _ = params;
        const idx = results[index].refinement.?;
        refinements.at(idx).scalar.undefined = .{ .defined = {} };
    }

    pub fn optional_payload(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.OptionalPayload) !void {
        _ = ctx;
        _ = results;
        _ = index;
        _ = refinements;
        // For .eidx: result shares source's entity, undefined state already correct
        if (params.src == .interned) {
            @panic("optional_payload: interned source unimplemented");
        }
    }

    /// For interned (compile-time constant) args, set everything to defined.
    /// For eidx args, the entity was copied with its existing state - no change needed.
    pub fn arg(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Arg) !void {
        _ = ctx;
        switch (params.value) {
            .interned => {
                // Compile-time constants are always defined
                const idx = results[index].refinement.?;
                setDefinedRecursive(refinements, idx);
            },
            .eidx => {
                // Runtime value - undefined state was already copied from caller
            },
            .other => {},
        }
    }

    /// br sets refinement on its target block, not on itself.
    /// When it creates scalars for the block, we need to set their undefined state.
    pub fn br(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Br) !void {
        _ = index;
        _ = ctx;
        // br sets refinement on self.block, not on its own index
        // When source is an eidx with existing refinement, undefined state is already set.
        // When br creates a new scalar (interned source), we need to set undefined.
        switch (params.src) {
            .eidx => {}, // Source has existing refinement with undefined state
            .interned => {
                const block_idx = results[params.block].refinement orelse return;
                setDefinedRecursive(refinements, block_idx);
            },
            .other => @panic("br: unexpected .other source"),
        }
    }

    /// Recursively set undefined state on a refinement and its children.
    /// Only called when storing an undefined value (src type is .undefined).
    fn setUndefinedRecursive(refinements: *Refinements, idx: EIdx, undef_state: Undefined) void {
        switch (refinements.at(idx).*) {
            .scalar => |*s| s.undefined = undef_state,
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
        switch (ty) {
            .undefined => {
                // Mark as undefined - don't recurse further, undefined is terminal
                const undef_state: Undefined = .{ .undefined = .{ .meta = ctx.meta } };
                setUndefinedRecursive(refinements, idx, undef_state);
            },
            .@"struct" => |fields| {
                // Apply field-level undefined state
                switch (refinements.at(idx).*) {
                    .@"struct" => |s| {
                        for (fields, 0..) |field, i| {
                            if (i < s.fields.len) {
                                applyInternedType(refinements, s.fields[i], field.ty, ctx);
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
                    .scalar => |*s| s.undefined = .{ .defined = {} },
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
            .@"union" => |fields| {
                // Apply field-level undefined state for unions
                switch (refinements.at(idx).*) {
                    .@"union" => |*u| {
                        // Mark union itself as defined (since we're storing a value, not undefined)
                        u.analyte.undefined = .{ .defined = {} };
                        for (fields, 0..) |field, i| {
                            if (i < u.fields.len) {
                                if (u.fields[i]) |field_idx| {
                                    applyInternedType(refinements, field_idx, field.ty, ctx);
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

    pub fn store(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Store) !void {
        _ = index;

        const ptr = params.ptr orelse @panic("store: ptr is null (interned/global) - not yet supported");
        // Follow pointer to get to pointee (local only - propagation happens on function close)
        const ptr_idx = results[ptr].refinement orelse @panic("store: ptr inst has no refinement");
        // Follow pointer to get pointee - panic on unexpected types
        const pointee_idx = refinements.at(ptr_idx).pointer.to;

        // Check if source is an undefined type (interned with .undefined wrapper)
        const is_undef = switch (params.src) {
            .interned => |ty| ty == .undefined,
            else => false,
        };

        if (is_undef) {
            // Undefined stores: mark pointee and all children as undefined (recursive)
            // Capture name from the destination pointer instruction
            const undef_state: Undefined = .{ .undefined = .{ .meta = ctx.meta, .name_when_set = results[ptr].name } };
            setUndefinedRecursive(refinements, pointee_idx, undef_state);
        } else {
            // Defined stores: update the pointee based on source type
            switch (params.src) {
                .eidx => |src_idx| {
                    // Runtime source - connect pointee to source's entity
                    const src_ref = results[src_idx].refinement orelse {
                        // Source has no refinement - just mark as defined
                        switch (refinements.at(pointee_idx).*) {
                            .scalar => |*s| s.undefined = .{ .defined = {} },
                            .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                            else => {},
                        }
                        return;
                    };
                    // When storing a pointer value, update the destination's `to` field
                    // so loads through the destination will reach the same target
                    switch (refinements.at(pointee_idx).*) {
                        .scalar => |*s| s.undefined = .{ .defined = {} },
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
                                .scalar => |*s| s.undefined = .{ .defined = {} },
                                .pointer => |*p| p.analyte.undefined = .{ .defined = {} },
                                else => {},
                            }
                        },
                        .errorunion => |e| {
                            switch (refinements.at(e.to).*) {
                                .scalar => |*s| s.undefined = .{ .defined = {} },
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
                    applyInternedType(refinements, pointee_idx, ty, ctx);
                },
                .other => {
                    // Other sources (globals) - just mark as defined
                    setDefinedRecursive(refinements, pointee_idx);
                },
            }
        }
    }

    pub fn load(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.Load) !void {
        const ptr = params.ptr orelse @panic("load: ptr is null (interned/global) - not yet supported");
        const ptr_idx = results[ptr].refinement orelse @panic("load: ptr inst has no refinement");
        // Follow pointer to get to pointee
        const pointee_idx = refinements.at(ptr_idx).pointer.to;
        switch (refinements.at(pointee_idx).*) {
            .scalar => |s| {
                // undefined is null when value wasn't allocated through our tracked mechanisms
                // (e.g., external data, FFI). No undefined tracking available - skip.
                const undef = s.undefined orelse return;
                switch (undef) {
                    .undefined => return undef.reportUseBeforeAssign(ctx),
                    .inconsistent => return undef.reportInconsistentBranches(ctx),
                    .defined => {
                        // Propagate defined state to the loaded value
                        const idx = results[index].refinement.?;
                        refinements.at(idx).scalar.undefined = .{ .defined = {} };
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
                            .scalar => |*s| s.undefined = .{ .defined = {} },
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
                        const undef = s.undefined orelse return;
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
                        const undef = s.undefined orelse return;
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

    pub fn struct_field_val(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, params: tag.StructFieldVal) !void {
        const result_idx = results[index].refinement.?;

        const operand = params.operand orelse {
            // Interned/global struct - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
            return;
        };
        const container_ref = results[operand].refinement orelse {
            // No struct refinement - result was created as fresh scalar, set to defined
            refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
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
                }
            },
            else => {
                // Not a struct or union - result was created as fresh scalar, set to defined
                refinements.at(result_idx).scalar.undefined = .{ .defined = {} };
            },
        }
    }

    fn checkFieldUndefined(refinements: *Refinements, field_idx: Refinements.EIdx, ctx: *Context) !void {
        switch (refinements.at(field_idx).*) {
            .scalar => |sc| {
                const undef = sc.undefined orelse @panic("struct_field_val: field scalar has no undefined state");
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
                const true_undef = true_ref.scalar.undefined orelse return;
                const false_undef = false_ref.scalar.undefined orelse return;
                s.undefined = mergeUndefinedStates(ctx, true_undef, false_undef);
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

/// Validate that refinements conform to undefined tracking rules:
/// - For .optional and .struct, the top-level analyte.undefined must be null
/// - For .scalar and .pointer, the analyte.undefined must be set (not null)
pub fn testValid(refinements: *Refinements) void {
    if (!debug) return;
    for (refinements.list.items) |ref| {
        switch (ref) {
            .scalar => |s| {
                if (s.undefined == null) {
                    @panic("ScalarMissingUndefinedState");
                }
            },
            .pointer => |p| {
                if (p.analyte.undefined == null) {
                    @panic("PointerMissingUndefinedState");
                }
            },
            .optional => |o| {
                if (o.analyte.undefined != null) {
                    @panic("OptionalContainerHasUndefinedState");
                }
            },
            .errorunion => |e| {
                if (e.analyte.undefined != null) {
                    @panic("ErrorUnionRootHasUndefinedState");
                }
            },
            .@"struct" => |s| {
                if (s.analyte.undefined != null) {
                    @panic("StructContainerHasUndefinedState");
                }
            },
            else => {},
        }
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
    try Inst.apply(state, 1, .{ .alloc = .{ .ty = .{ .scalar = {} } } });

    // alloc creates pointer; pointee type is determined by .ty parameter
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(pointee_idx).*));
    testValid(&refinements);
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
    try Inst.apply(state, 1, .{ .alloc_create = .{ .allocator_type = "PageAllocator", .ty = .{ .scalar = {} } } });

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
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .undefined = &.{ .scalar = {} } } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
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
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .scalar = {} } } } });

    // Check the pointee's undefined state
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    const undef = refinements.at(pointee_idx).scalar.undefined.?;
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
    try Inst.apply(state, 0, .{ .store_safe = .{ .ptr = 1, .src = .{ .interned = .{ .null = &.{ .scalar = {} } } } } });

    // Check the pointee is an optional
    const pointee_idx = refinements.at(results[1].refinement.?).pointer.to;
    try std.testing.expectEqual(.optional, std.meta.activeTag(refinements.at(pointee_idx).*));

    // Check the optional's inner value is a defined scalar
    const inner_idx = refinements.at(pointee_idx).optional.to;
    try std.testing.expectEqual(.scalar, std.meta.activeTag(refinements.at(inner_idx).*));
    try std.testing.expectEqual(.defined, std.meta.activeTag(refinements.at(inner_idx).scalar.undefined.?));
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = .{ .undefined = .{ .meta = .{
        .function = "test_func",
        .file = "test.zig",
        .line = 1,
    } } } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

    try std.testing.expectError(
        error.UseBeforeAssign,
        Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } }),
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = .{ .defined = {} } } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

    // Set up result for the load instruction (index 0)
    _ = try Inst.clobberInst(&refinements, &results, 0, .{ .scalar = .{} });

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } });
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
    const pointee_idx = try refinements.appendEntity(.{ .scalar = .{ .undefined = null } });
    _ = try Inst.clobberInst(&refinements, &results, 1, .{ .pointer = .{ .analyte = .{}, .to = pointee_idx } });

    try Undefined.load(&results, 0, &ctx, &refinements, .{ .ptr = 1, .ty = .{ .scalar = {} } });
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
