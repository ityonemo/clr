const std = @import("std");
const Allocator = std.mem.Allocator;

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

    scalar: Analyte,
    pointer: Indirected,
    optional: Indirected,
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": void, // unused, for now, temporarily void. Will be a slice of EIdx.
    @"union": void, // unused, for now, temporarily void. Will be a slice EIdx.
    retval_future: void, // special-case for retval slots before a return has been called.
    future: void, // this special-case for values which will contain a refinement type, but it isn't set at this
    // particular point in time.
    unimplemented: void, // this means we have an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.

    /// clobbers one refinement over another.  The dst structure must be identical.
    /// TODO: In some cases, "deeper specification" may be added.
    /// NOTE: For .future destinations, use clobber_structured_idx instead because
    /// clobber_future may reallocate dst_list, invalidating the dst pointer.
    pub fn clobber_structured(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements) void {
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => recurse_indirected(dst, dst_list, src, src_list, .optional),
            .retval_future => @panic("cannot copy from .retval_future"),
            .region => recurse_indirected(dst, dst_list, src, src_list, .region),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // future: use index-based version to avoid stale pointer after reallocation
            .future => @panic("clobber_structured: use clobber_structured_idx for .future destinations"),
            // following two types don't have any metadata associated with it.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
        }
    }

    /// clobbers one refinement over another, using index for destination.
    /// Use this when dst might be .future (which requires reallocation-safe handling).
    pub fn clobber_structured_idx(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) void {
        const dst = dst_list.at(dst_idx);
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => recurse_indirected(dst, dst_list, src, src_list, .optional),
            .retval_future => @panic("cannot copy from .retval_future"),
            .region => recurse_indirected(dst, dst_list, src, src_list, .region),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // future: use index-based clobber to handle reallocation
            .future => clobber_future(dst_idx, dst_list, src, src_list),
            // following two types don't have any metadata associated with it.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
        }
    }

    /// Clobber a .future slot with a src refinement, copying the structure without checks.
    /// Takes dst_idx instead of pointer because copy_to may reallocate dst_list.
    fn clobber_future(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) void {
        const same_list = src_list == dst_list;
        // Note: We must compute the new value BEFORE getting a pointer to dst,
        // because copy_to may reallocate dst_list's backing memory.
        const new_value: Refinement = switch (src) {
            .scalar => src,
            .pointer => |p| .{ .pointer = .{
                .analyte = p.analyte,
                .to = if (same_list) p.to else copy_to(src_list.at(p.to).*, src_list, dst_list) catch @panic("out of memory"),
            } },
            .optional => |o| .{ .optional = .{
                .analyte = o.analyte,
                .to = if (same_list) o.to else copy_to(src_list.at(o.to).*, src_list, dst_list) catch @panic("out of memory"),
            } },
            .region => |r| .{ .region = .{
                .analyte = r.analyte,
                .to = if (same_list) r.to else copy_to(src_list.at(r.to).*, src_list, dst_list) catch @panic("out of memory"),
            } },
            .@"struct" => .{ .@"struct" = {} },
            .@"union" => .{ .@"union" = {} },
            .unimplemented => .{ .unimplemented = {} },
            .void => .{ .void = {} },
            .future => @panic("cannot copy from .future"),
            .retval_future => @panic("cannot copy from .retval_future"),
        };
        // Now safe to get pointer and assign
        dst_list.at(dst_idx).* = new_value;
    }

    fn recurse_indirected(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements, comptime tag: anytype) void {
        if (src != tag) std.debug.panic("clobber mismatch: src is .{s} and dst is .{s}", .{ @tagName(src), @tagName(tag) });
        // copy over analytes.
        @field(dst, @tagName(tag)).analyte = @field(src, @tagName(tag)).analyte;
        const new_dst = dst_list.at(@field(dst, @tagName(tag)).to);
        const new_src = src_list.at(@field(src, @tagName(tag)).to);
        clobber_structured(new_dst, dst_list, new_src.*, src_list);
    }

    fn recurse_fields(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements, comptime tag: anytype) void {
        // currently do nothing, since struct and union fields aren't implemented.
        _ = dst;
        _ = dst_list;
        _ = src;
        _ = src_list;
        _ = tag;
    }

    /// Recursively copies a refinement into dst_list.  Returns the entity index of the result.
    pub fn copy_to(src: Refinement, src_list: *Refinements, dst_list: *Refinements) !EIdx {
        std.debug.assert(src_list != dst_list); // use index directly if same list
        return switch (src) {
            .scalar => try dst_list.appendEntity(src),
            .pointer => try src.copy_to_indirected(src_list, dst_list, .pointer),
            .optional => try src.copy_to_indirected(src_list, dst_list, .optional),
            .retval_future => @panic("cannot copy from .retval_future"),
            .future => @panic("cannot copy from .future"),
            .region => try src.copy_to_indirected(src_list, dst_list, .region),
            .@"struct" => try src.copy_to_fields(src_list, dst_list, .@"struct"),
            .@"union" => try src.copy_to_fields(src_list, dst_list, .@"union"),
            // following two types don't have any metadata associated with it.
            .unimplemented => try dst_list.appendEntity(.{ .unimplemented = {} }),
            .void => try dst_list.appendEntity(.{ .void = {} }),
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
        // this is just a placeholder for what will be an actual implementation when fielded refinements are implemented
        _ = src;
        _ = src_list;
        const to_insert: Refinement = @unionInit(Refinement, @tagName(tag), {});
        return dst_list.appendEntity(to_insert);
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
pub fn clone(self: *Refinements, allocator: Allocator) !Refinements {
    var new = Refinements.init(allocator);
    try new.list.appendSlice(self.list.items);
    return new;
}
