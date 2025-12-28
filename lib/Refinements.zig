const std = @import("std");
const Allocator = std.mem.Allocator;
const Meta = @import("Meta.zig");

/// Entity Index - index into the Refinements entity table.
pub const EIdx = u32;

pub const Analyte = @import("Analyte.zig");

/// Refinement tracks the type structure of a value along with analysis state.
/// This is like a refinement type - the base type plus predicates/properties.
/// EIdx is used for anything that needs indirection (pointers, optionals, etc).
pub const Refinement = union(enum) {
    pub const Indirected = struct {
        analyte: Analyte,
        to: EIdx,
    };

    pub const Struct = struct {
        /// Analyte for tracking on the whole struct
        analyte: Analyte = .{},
        /// Field refinement indices - each field has its own refinement
        fields: []const EIdx,
        /// Field names - each field has an optional name (null for tuple fields)
        /// Strings are static (embedded in generated .air.zig) so no allocation needed
        field_names: []const ?[]const u8 = &.{},
    };

    pub const Union = struct {
        /// Analyte for tracking on the whole union
        analyte: Analyte = .{},
        /// Field refinement indices - each field has its own refinement.  Any field
        /// that is null, is NOT active in this variable. Mutable because set_union_tag
        /// needs to activate/deactivate fields.
        fields: []?EIdx,
        /// Field names - each field has an optional name (null for tuple fields)
        /// Strings are static (embedded in generated .air.zig) so no allocation needed
        field_names: []const ?[]const u8 = &.{},
    };

    scalar: Analyte,
    pointer: Indirected,
    optional: Indirected,
    errorunion: Indirected,
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": Struct,
    @"union": Union,
    noreturn: void, // specific return value for error paths.
    retval_future: void, // special-case for retvals before a return has been called.
    unimplemented: void, // this is the result of an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.

    /// clobbers one refinement over another.  The dst structure must be identical.
    /// TODO: In some cases, "deeper specification" may be added.
    /// NOTE: For .retval_future destinations, use clobber_structured_idx instead because
    /// clobber_retval_future may reallocate dst_list, invalidating the dst pointer.
    pub fn clobber_structured(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            .errorunion => try recurse_indirected(dst, dst_list, src, src_list, .errorunion),
            .retval_future => @panic("clobber_structured: use clobber_structured_idx for .retval_future destinations"),
            .region => @panic("regions not implemented yet"),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    /// clobbers one refinement over another, using index for destination.
    /// Use this when dst might be .retval_future (which requires reallocation-safe handling).
    pub fn clobber_structured_idx(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        const dst = dst_list.at(dst_idx);
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            .errorunion => try recurse_indirected(dst, dst_list, src, src_list, .errorunion),
            // retval_future is a placeholder for return values, to be filled in by ret_safe
            .retval_future => try clobber_retval_future(dst_idx, dst_list, src, src_list),
            .region => @panic("regions not implemented yet"),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    /// Clobber a .retval_future slot with a src refinement, copying the structure without checks.
    /// Takes dst_idx instead of pointer because copy_to may reallocate dst_list.
    fn clobber_retval_future(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        const same_list = src_list == dst_list;
        // Note: We must compute the new value BEFORE getting a pointer to dst,
        // because copy_to may reallocate dst_list's backing memory.
        const new_value: Refinement = switch (src) {
            .scalar => src,
            .pointer => |p| .{ .pointer = .{
                .analyte = p.analyte,
                .to = if (same_list) p.to else try copy_to(src_list.at(p.to).*, src_list, dst_list),
            } },
            .optional => |o| .{ .optional = .{
                .analyte = o.analyte,
                .to = if (same_list) o.to else try copy_to(src_list.at(o.to).*, src_list, dst_list),
            } },
            .errorunion => |e| .{ .errorunion = .{
                .analyte = e.analyte,
                .to = if (same_list) e.to else try copy_to(src_list.at(e.to).*, src_list, dst_list),
            } },
            .region => @panic("regions not implemented yet"),
            .@"struct" => |s| blk: {
                const allocator = dst_list.list.allocator;
                const new_fields = try allocator.alloc(EIdx, s.fields.len);
                if (same_list) {
                    // Same list: just copy field indices, they already reference valid entities
                    @memcpy(new_fields, s.fields);
                } else {
                    // Different lists: need to copy field entities too
                    for (s.fields, 0..) |field_idx, i| {
                        new_fields[i] = try copy_to(src_list.at(field_idx).*, src_list, dst_list);
                    }
                }
                // Copy field names if present
                const new_field_names = if (s.field_names.len > 0) names_blk: {
                    const names = try allocator.alloc(?[]const u8, s.field_names.len);
                    @memcpy(names, s.field_names);
                    break :names_blk names;
                } else &.{};
                break :blk .{ .@"struct" = .{ .analyte = s.analyte, .fields = new_fields, .field_names = new_field_names } };
            },
            .@"union" => |u| blk: {
                const allocator = dst_list.list.allocator;
                const new_fields = try allocator.alloc(?EIdx, u.fields.len);
                if (same_list) {
                    @memcpy(new_fields, u.fields);
                } else {
                    for (u.fields, 0..) |field_idx_opt, i| {
                        new_fields[i] = if (field_idx_opt) |field_idx|
                            try copy_to(src_list.at(field_idx).*, src_list, dst_list)
                        else
                            null;
                    }
                }
                const new_field_names = if (u.field_names.len > 0) names_blk: {
                    const names = try allocator.alloc(?[]const u8, u.field_names.len);
                    @memcpy(names, u.field_names);
                    break :names_blk names;
                } else &.{};
                break :blk .{ .@"union" = .{ .analyte = u.analyte, .fields = new_fields, .field_names = new_field_names } };
            },
            .unimplemented => .{ .unimplemented = {} },
            .void => .{ .void = {} },
            .noreturn => .{ .noreturn = {} },
            .retval_future => @panic("cannot copy from .retval_future"),
        };
        // Now safe to get pointer and assign
        dst_list.at(dst_idx).* = new_value;
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

        // Copy analyte - deep copy only when crossing between different refinement tables
        var new_analyte = @field(src, @tagName(tag)).analyte;
        if (tag == .@"union" and src_list != dst_list) {
            // Different tables - deep copy variant_safety.active_metas to avoid sharing
            if (@field(src, @tagName(tag)).analyte.variant_safety) |vs| {
                const allocator = dst_list.list.allocator;
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
                                const new_idx = copy_to(src_list.at(src_field.?).*, src_list, dst_list) catch @panic("out of memory");
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

    /// Recursively copies a refinement into dst_list.  Returns the entity index of the result.
    pub fn copy_to(src: Refinement, src_list: *Refinements, dst_list: *Refinements) !EIdx {
        std.debug.assert(src_list != dst_list); // use index directly if same list
        return switch (src) {
            .scalar => try dst_list.appendEntity(src),
            .pointer => try src.copy_to_indirected(src_list, dst_list, .pointer),
            .optional => try src.copy_to_indirected(src_list, dst_list, .optional),
            .errorunion => try src.copy_to_indirected(src_list, dst_list, .errorunion),
            .retval_future => @panic("cannot copy from .retval_future"),
            .region => @panic("regions not implemented yet"),
            .@"struct" => try src.copy_to_fields(src_list, dst_list, .@"struct"),
            .@"union" => try src.copy_to_fields(src_list, dst_list, .@"union"),
            // following types don't have any metadata associated with them.
            .unimplemented => try dst_list.appendEntity(.{ .unimplemented = {} }),
            .void => try dst_list.appendEntity(.{ .void = {} }),
            .noreturn => try dst_list.appendEntity(.{ .noreturn = {} }),
        };
    }

    fn copy_to_indirected(src: Refinement, src_list: *Refinements, dst_list: *Refinements, comptime tag: anytype) error{OutOfMemory}!EIdx {
        const src_inner_idx = @field(src, @tagName(tag)).to;
        const dst_inner_idx = try copy_to(src_list.at(src_inner_idx).*, src_list, dst_list);
        const to_insert: Refinement = @unionInit(Refinement, @tagName(tag), .{
            .to = dst_inner_idx,
            .analyte = @field(src, @tagName(tag)).analyte,
        });
        return dst_list.appendEntity(to_insert);
    }

    fn copy_to_fields(src: Refinement, src_list: *Refinements, dst_list: *Refinements, comptime tag: anytype) error{OutOfMemory}!EIdx {
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
            const new_idx = try copy_to(src_list.at(src_field_idx).*, src_list, dst_list);
            new_fields[i] = if (tag == .@"union") new_idx else new_idx;
        }

        // Copy field names if present
        const new_field_names = if (src_data.field_names.len > 0) blk: {
            const names = try allocator.alloc(?[]const u8, src_data.field_names.len);
            @memcpy(names, src_data.field_names);
            break :blk names;
        } else &.{};

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
            .field_names = new_field_names,
        }));
    }
};

const Refinements = @This();

/// Entity table that stores all Refinement values.
/// Each entity is indexed by EIdx. Entities can reference other entities
/// via EIdx (e.g., a pointer entity references its pointee entity).
list: std.array_list.AlignedManaged(Refinement, null),

pub fn init(allocator: Allocator) Refinements {
    return .{ .list = std.array_list.AlignedManaged(Refinement, null).init(allocator) };
}

pub fn deinit(self: *Refinements) void {
    const allocator = self.list.allocator;
    // Free any struct and union field allocations
    for (self.list.items) |item| {
        switch (item) {
            inline .@"struct", .@"union" => |data| {
                allocator.free(data.fields);
                if (data.field_names.len > 0) {
                    allocator.free(data.field_names);
                }
            },
            else => {},
        }
    }
    self.list.deinit();
}

/// Get Refinement by entity index. Crashes if index is out of bounds.
pub fn at(self: *Refinements, index: EIdx) *Refinement {
    return &self.list.items[index];
}

/// Append a new entity with the given value and return its index.
pub fn appendEntity(self: *Refinements, value: Refinement) !EIdx {
    const idx: EIdx = @intCast(self.list.items.len);
    try self.list.append(value);
    return idx;
}

/// Deep clone the entire refinements table.
/// EIdx references remain valid because indices are preserved.
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
                // Deep copy field names if present
                const new_field_names = if (data.field_names.len > 0) blk: {
                    const names = try allocator.alloc(?[]const u8, data.field_names.len);
                    @memcpy(names, data.field_names);
                    break :blk names;
                } else &.{};
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
                    .field_names = new_field_names,
                }));
            },
            else => try new.list.append(item),
        }
    }
    return new;
}

const undefined_analysis = @import("analysis/undefined.zig");
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
