const std = @import("std");
const Allocator = std.mem.Allocator;
const Meta = @import("Meta.zig");

/// Entity Index - index into the Refinements entity table.
pub const EIdx = u32;

/// Type ID - InternPool index identifying a type for type safety checks.
pub const Tid = u32;

/// Global ID - unique identifier for provenance tracking across function boundaries.
pub const Gid = u32;

pub const Analyte = @import("Analyte.zig");

/// Refinement tracks the type structure of a value along with analysis state.
/// This is like a refinement type - the base type plus predicates/properties.
/// GID is used for anything that needs indirection (pointers, optionals, etc).
pub const Refinement = union(enum) {
    pub const Scalar = struct {
        gid: Gid = 0,
        analyte: Analyte,
        type_id: Tid,
    };

    pub const Indirected = struct {
        gid: Gid = 0,
        analyte: Analyte,
        type_id: Tid,
        to: Gid,
    };

    pub const Struct = struct {
        gid: Gid = 0,
        /// Analyte for tracking on the whole struct
        analyte: Analyte = .{},
        /// Field refinement GIDs - each field has its own refinement
        /// To obtain the fieldname use field_id = ctx.getFieldId(type_id, field_index) and ctx.getName(field_id)
        fields: []const Gid,
        type_id: Tid,
    };

    pub const Union = struct {
        gid: Gid = 0,
        /// Analyte for tracking on the whole union
        analyte: Analyte = .{},
        /// Field refinement GIDs - each field has its own refinement.  Any field
        /// that is null, is NOT active in this variable. Mutable because set_union_tag
        /// needs to activate/deactivate fields.
        /// To obtain the field name use field_id = ctx.getFieldId(type_id, field_index) and ctx.getName(field_id)
        fields: []?Gid,
        type_id: Tid,
    };

    /// AllocatorRef refinement tracks allocator identity for mismatch detection.
    /// The type_id uniquely identifies the allocator type (e.g., PageAllocator vs GPA).
    pub const AllocatorRef = struct {
        gid: Gid = 0,
        analyte: Analyte = .{},
        type_id: Tid,
    };

    scalar: Scalar,
    pointer: Indirected,
    optional: Indirected,
    errorunion: Indirected,
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": Struct,
    @"union": Union,
    allocator: AllocatorRef, // tracks allocator identity for mismatch detection
    noreturn: void, // specific return value for error paths.
    unimplemented: void, // this is the result of an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.

    /// clobbers one refinement over another.  The dst structure must be identical.
    /// TODO: In some cases, "deeper specification" may be added.
    pub fn clobber_structured(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .allocator => {
                if (src != .allocator) std.debug.panic("clobber mismatch: src is .{s} and dst is .allocator", .{@tagName(src)});
                dst.allocator = src.allocator;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            .errorunion => try recurse_indirected(dst, dst_list, src, src_list, .errorunion),
            .region => @panic("regions not implemented yet"),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    /// clobbers one refinement over another, using GID for destination.
    pub fn clobber_structured_idx(dst_gid: Gid, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        const dst = dst_list.at(dst_gid);
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .allocator => {
                if (src != .allocator) std.debug.panic("clobber mismatch: src is .{s} and dst is .allocator", .{@tagName(src)});
                dst.allocator = src.allocator;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            .errorunion => try recurse_indirected(dst, dst_list, src, src_list, .errorunion),
            .region => @panic("regions not implemented yet"),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    fn recurse_indirected(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements, comptime tag: anytype) error{OutOfMemory}!void {
        if (src != tag) std.debug.panic("clobber mismatch: src is .{s} and dst is .{s}", .{ @tagName(src), @tagName(tag) });
        // copy over analytes.
        @field(dst, @tagName(tag)).analyte = @field(src, @tagName(tag)).analyte;
        const new_dst = dst_list.at(@field(dst, @tagName(tag)).to);
        const new_src = src_list.at(@field(src, @tagName(tag)).to);
        try clobber_structured(new_dst, dst_list, new_src.*, src_list);
    }

    fn recurse_fields(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements, comptime tag: anytype) void {
        if (src != tag) std.debug.panic("clobber mismatch: src is .{s} and dst is .{s}", .{ @tagName(src), @tagName(tag) });

        const allocator = dst_list.list.allocator;

        // For unions, free old active_metas before overwriting
        if (tag == .@"union") {
            if (@field(dst, @tagName(tag)).analyte.variant_safety) |old_vs| {
                allocator.free(old_vs.active_metas);
            }
        }

        // Copy analyte - deep copy only when crossing between different refinement tables
        var new_analyte = @field(src, @tagName(tag)).analyte;
        if (tag == .@"union" and src_list != dst_list) {
            // Different tables - deep copy variant_safety.active_metas to avoid sharing
            if (@field(src, @tagName(tag)).analyte.variant_safety) |vs| {
                const new_active_metas = allocator.alloc(?Meta, vs.active_metas.len) catch @panic("out of memory");
                @memcpy(new_active_metas, vs.active_metas);
                new_analyte.variant_safety = .{ .active_metas = new_active_metas };
            }
        }
        // Same table: shallow copy is fine - mutations should be visible
        @field(dst, @tagName(tag)).analyte = new_analyte;

        // Recurse into each field
        const dst_fields = @field(dst, @tagName(tag)).fields;
        const src_fields = @field(src, @tagName(tag)).fields;
        const mutable_dst_fields = @constCast(dst_fields);
        for (mutable_dst_fields, src_fields, 0..) |*dst_field, src_field, i| {
            // if we are in a union, we'll have to unwrap only fields that
            // are non-null, otherwise jump.  For structs no need to unwrap.
            const new_dst_idx, const new_src_idx = new: {
                if (tag == .@"union") {
                    // If src has a field that dst doesn't, need to check if it's active
                    if (src_field != null and dst_field.* == null) {
                        // Check if this field is active in variant_safety
                        if (new_analyte.variant_safety) |vs| {
                            if (vs.active_metas[i] != null) {
                                // Active field in src but not in dst - copy it
                                const new_idx = copyTo(src_list.at(src_field.?).*, src_list, dst_list) catch @panic("out of memory");
                                dst_field.* = new_idx;
                            }
                        }
                        continue;
                    }
                    if (dst_field.* == null or src_field == null) continue;
                    break :new .{ dst_field.*.?, src_field.? };
                } else {
                    break :new .{ dst_field.*, src_field };
                }
            };

            const new_dst = dst_list.at(new_dst_idx);
            const new_src = src_list.at(new_src_idx);
            clobber_structured(new_dst, dst_list, new_src.*, src_list) catch @panic("out of memory");
        }
    }

    /// Cross-table copy: copies refinement from src_list to dst_list.
    /// Uses noalias to assert tables are different.
    /// Follows semideep copy rules - pointers reference same pointee across tables.
    pub fn copyTo(src: Refinement, noalias src_list: *Refinements, noalias dst_list: *Refinements) !Gid {
        return switch (src) {
            .scalar => try dst_list.appendEntity(src),
            .allocator => try dst_list.appendEntity(src),
            .pointer => try src.copyToIndirected(src_list, dst_list, .pointer),
            .optional => try src.copyToIndirected(src_list, dst_list, .optional),
            .errorunion => try src.copyToIndirected(src_list, dst_list, .errorunion),
            .region => @panic("regions not implemented yet"),
            .@"struct" => try src.copyToFields(src_list, dst_list, .@"struct"),
            .@"union" => try src.copyToFields(src_list, dst_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => try dst_list.appendEntity(.{ .unimplemented = {} }),
            .void => try dst_list.appendEntity(.{ .void = {} }),
            .noreturn => try dst_list.appendEntity(.{ .noreturn = {} }),
        };
    }

    fn copyToIndirected(src: Refinement, noalias src_list: *Refinements, noalias dst_list: *Refinements, comptime tag: anytype) error{OutOfMemory}!Gid {
        const src_inner_idx = @field(src, @tagName(tag)).to;
        const dst_inner_idx = try copyTo(src_list.at(src_inner_idx).*, src_list, dst_list);
        const to_insert: Refinement = @unionInit(Refinement, @tagName(tag), .{
            .to = dst_inner_idx,
            .analyte = @field(src, @tagName(tag)).analyte,
            .type_id = @field(src, @tagName(tag)).type_id,
        });
        return dst_list.appendEntity(to_insert);
    }

    /// Get the GID of this refinement. Unreachable for void/noreturn/unimplemented.
    pub fn getGid(self: Refinement) Gid {
        return switch (self) {
            .void, .noreturn, .unimplemented => unreachable,
            inline else => |data| data.gid,
        };
    }

    fn copyToFields(src: Refinement, noalias src_list: *Refinements, noalias dst_list: *Refinements, comptime tag: anytype) error{OutOfMemory}!Gid {
        const allocator = dst_list.list.allocator;
        const src_data = @field(src, @tagName(tag));
        const src_fields = src_data.fields;

        // Allocate new fields array (same type as source)
        const new_fields = try allocator.alloc(@TypeOf(src_fields[0]), src_fields.len);
        for (src_fields, 0..) |src_field, i| {
            // For unions, fields are optional; for structs, they're not
            const src_field_idx = if (tag == .@"union")
                src_field orelse {
                    new_fields[i] = null;
                    continue;
                }
            else
                src_field;
            const new_idx = try copyTo(src_list.at(src_field_idx).*, src_list, dst_list);
            new_fields[i] = if (tag == .@"union") new_idx else new_idx;
        }

        // Deep copy analyte - variant_safety.active_metas is a slice that must not be shared
        var new_analyte = src_data.analyte;
        if (tag == .@"union") {
            if (src_data.analyte.variant_safety) |vs| {
                const new_active_metas = try allocator.alloc(?Meta, vs.active_metas.len);
                @memcpy(new_active_metas, vs.active_metas);
                new_analyte.variant_safety = .{ .active_metas = new_active_metas };
            }
        }

        return dst_list.appendEntity(@unionInit(Refinement, @tagName(tag), .{
            .analyte = new_analyte,
            .fields = new_fields,
            .type_id = src_data.type_id,
        }));
    }
};

const Refinements = @This();

/// Entity table that stores all Refinement values.
/// Each entity is indexed by GID. Entities can reference other entities
/// via GID (e.g., a pointer entity references its pointee entity).
list: std.array_list.AlignedManaged(Refinement, null),

pub fn init(allocator: Allocator) Refinements {
    return .{ .list = std.array_list.AlignedManaged(Refinement, null).init(allocator) };
}

pub fn deinit(self: *Refinements) void {
    const allocator = self.list.allocator;
    // Free any struct and union field allocations
    for (self.list.items) |item| {
        switch (item) {
            inline .@"struct", .@"union" => |data, tag| {
                allocator.free(data.fields);
                // Free variant_safety.active_metas for unions
                if (tag == .@"union") {
                    if (data.analyte.variant_safety) |vs| {
                        allocator.free(vs.active_metas);
                    }
                }
            },
            else => {},
        }
    }
    self.list.deinit();
}

/// Get Refinement by GID. Crashes if GID is out of bounds.
/// Since GID == EIdx at creation and entities don't move, this is O(1).
pub fn at(self: *Refinements, gid: Gid) *Refinement {
    return &self.list.items[gid];
}

/// Find entity by GID. Returns the GID if found, null otherwise.
/// Note: After branch merging, an entity may not exist in this table
/// even if it was created elsewhere with that GID.
/// Since GID == EIdx at creation, this can often be O(1) with a direct lookup.
pub fn findByGid(self: *Refinements, gid: Gid) ?Gid {
    // Fast path: check if gid is a valid index and entity at that index has matching gid
    if (gid < self.list.items.len) {
        switch (self.list.items[gid]) {
            .void, .noreturn, .unimplemented => {},
            inline else => |data| if (data.gid == gid) return gid,
        }
    }
    // Slow path: linear scan (needed after deduplication/merge)
    for (self.list.items, 0..) |ref, idx| {
        switch (ref) {
            .void, .noreturn, .unimplemented => {},
            inline else => |data| if (data.gid == gid) return @intCast(idx),
        }
    }
    return null;
}

/// Append a new entity with the given value and return its GID.
/// GID is automatically assigned as the entity's index (GID = EIdx at creation).
pub fn appendEntity(self: *Refinements, value: Refinement) !Gid {
    const idx: Gid = @intCast(self.list.items.len);
    var entity = value;
    // GID = EIdx at creation time (stable identity across clones/merges)
    switch (entity) {
        .void, .noreturn, .unimplemented => {},
        inline else => |*data| data.gid = idx,
    }
    try self.list.append(entity);
    return idx;
}

/// Append a new entity with the given value, assigning a new global ID from the counter.
/// DEPRECATED: Use appendEntity instead, which auto-assigns GID = EIdx.
pub fn appendEntityWithGid(self: *Refinements, value: Refinement, gid_counter: *Gid) !Gid {
    const idx: Gid = @intCast(self.list.items.len);
    var entity = value;
    const gid = gid_counter.*;
    gid_counter.* += 1;
    switch (entity) {
        .void, .noreturn, .unimplemented => {},
        inline else => |*data| data.gid = gid,
    }
    try self.list.append(entity);
    return idx;
}

/// Deep clone the entire refinements table.
/// GID references remain valid because indices are preserved.
/// Struct and union field slices are deep copied so each refinements list owns its own memory.
pub fn clone(self: *Refinements, allocator: Allocator) !Refinements {
    var new = Refinements.init(allocator);
    try new.list.ensureTotalCapacity(self.list.items.len);
    for (self.list.items) |item| {
        switch (item) {
            inline .@"struct", .@"union" => |data, tag| {
                // Deep copy fields
                const new_fields = try allocator.alloc(@TypeOf(data.fields[0]), data.fields.len);
                @memcpy(new_fields, data.fields);
                // Deep copy analyte, including variant_safety.active_metas if present
                var new_analyte = data.analyte;
                if (tag == .@"union") {
                    if (data.analyte.variant_safety) |vs| {
                        const new_active_metas = try allocator.alloc(?Meta, vs.active_metas.len);
                        @memcpy(new_active_metas, vs.active_metas);
                        new_analyte.variant_safety = .{ .active_metas = new_active_metas };
                    }
                }
                try new.list.append(@unionInit(Refinement, @tagName(tag), .{
                    .analyte = new_analyte,
                    .fields = new_fields,
                    .type_id = data.type_id,
                }));
            },
            else => try new.list.append(item),
        }
    }
    return new;
}

/// Deep copy a Refinement value, allocating new memory for struct/union fields.
/// Use this when copying a refinement to another slot to avoid double-free.
pub fn deepCopyValue(self: *Refinements, src: Refinement) !Refinement {
    const allocator = self.list.allocator;
    return switch (src) {
        inline .@"struct", .@"union" => |data, tag| blk: {
            // Deep copy fields
            const new_fields = try allocator.alloc(@TypeOf(data.fields[0]), data.fields.len);
            @memcpy(new_fields, data.fields);
            // Deep copy analyte, including variant_safety.active_metas if present
            var new_analyte = data.analyte;
            if (tag == .@"union") {
                if (data.analyte.variant_safety) |vs| {
                    const new_active_metas = try allocator.alloc(?Meta, vs.active_metas.len);
                    @memcpy(new_active_metas, vs.active_metas);
                    new_analyte.variant_safety = .{ .active_metas = new_active_metas };
                }
            }
            break :blk @unionInit(Refinement, @tagName(tag), .{
                .analyte = new_analyte,
                .fields = new_fields,
                .type_id = data.type_id,
            });
        },
        else => src, // Non-container types can be shallow copied
    };
}

/// Semideep copy within same table: creates new entity, copying values but sharing pointer targets.
/// For struct/union, recursively copies value fields, but pointer fields reference same pointee.
/// Always deep copies variant_safety.active_metas.
pub fn semideepCopy(self: *Refinements, src_gid: Gid) error{OutOfMemory}!Gid {
    const src = self.at(src_gid).*;
    return self.semideepCopyRefinement(src);
}

fn semideepCopyRefinement(self: *Refinements, src: Refinement) error{OutOfMemory}!Gid {
    const allocator = self.list.allocator;
    return switch (src) {
        // Scalars and allocators: copy to new entity
        .scalar => try self.appendEntity(src),
        .allocator => try self.appendEntity(src),

        // Pointers: new pointer entity, same .to (reference existing pointee)
        .pointer => |p| try self.appendEntity(.{
            .pointer = .{
                .analyte = p.analyte,
                .type_id = p.type_id,
                .to = p.to, // Same pointee - this is the "boundary"
            },
        }),

        // Optionals/errorunions: recurse into payload (semideep copy the inner value)
        .optional => |o| blk: {
            const new_inner = try self.semideepCopy(o.to);
            break :blk try self.appendEntity(.{ .optional = .{
                .analyte = o.analyte,
                .type_id = o.type_id,
                .to = new_inner,
            } });
        },
        .errorunion => |e| blk: {
            const new_inner = try self.semideepCopy(e.to);
            break :blk try self.appendEntity(.{ .errorunion = .{
                .analyte = e.analyte,
                .type_id = e.type_id,
                .to = new_inner,
            } });
        },

        // Structs: new struct with semideep copied fields
        .@"struct" => |s| blk: {
            const new_fields = try allocator.alloc(Gid, s.fields.len);
            for (s.fields, 0..) |field_idx, i| {
                new_fields[i] = try self.semideepCopy(field_idx);
            }
            break :blk try self.appendEntity(.{ .@"struct" = .{
                .analyte = s.analyte,
                .fields = new_fields,
                .type_id = s.type_id,
            } });
        },

        // Unions: new union with semideep copied active fields
        .@"union" => |u| blk: {
            const new_fields = try allocator.alloc(?Gid, u.fields.len);
            for (u.fields, 0..) |field_idx_opt, i| {
                new_fields[i] = if (field_idx_opt) |field_idx|
                    try self.semideepCopy(field_idx)
                else
                    null;
            }
            // Always deep copy variant_safety.active_metas
            var new_analyte = u.analyte;
            if (u.analyte.variant_safety) |vs| {
                const new_active_metas = try allocator.alloc(?Meta, vs.active_metas.len);
                @memcpy(new_active_metas, vs.active_metas);
                new_analyte.variant_safety = .{ .active_metas = new_active_metas };
            }
            break :blk try self.appendEntity(.{ .@"union" = .{
                .analyte = new_analyte,
                .fields = new_fields,
                .type_id = u.type_id,
            } });
        },

        .region => @panic("regions not implemented yet"),

        // Simple types: just copy
        .unimplemented => try self.appendEntity(.{ .unimplemented = {} }),
        .void => try self.appendEntity(.{ .void = {} }),
        .noreturn => try self.appendEntity(.{ .noreturn = {} }),
    };
}

/// Create pointer entity referencing existing entity (for struct_field_ptr, bitcast on pointer, etc.)
pub fn createPointerTo(self: *Refinements, pointee_gid: Gid, analyte: Analyte) !Gid {
    return self.appendEntity(.{ .pointer = .{
        .analyte = analyte,
        .to = pointee_gid,
    } });
}

const undefined_analysis = @import("analysis/undefined_safety.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");
const null_safety_analysis = @import("analysis/null_safety.zig");
const variant_safety_analysis = @import("analysis/variant_safety.zig");

pub fn testValid(self: *Refinements) void {
    for (self.list.items, 0..) |refinement, i| {
        undefined_analysis.testValid(refinement, i);
        memory_safety_analysis.testValid(refinement);
        null_safety_analysis.testValid(refinement);
        variant_safety_analysis.testValid(refinement);
    }
}
