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

    /// this special-case for values which will contain a refinement type, but it isn't set at this
    /// particular point in time.  Note that the structure of the refinement cannot be derived from 
    /// zig's compiler type, since for example a "usize" might *actually* be a pointer, and should
    /// be treated as a pointer in refinements.
    pub const Future = struct {
        /// null if it is anonymous, otherwise contains the variable name.
        name: ?[]const u8,
        /// if the future gets cancelled due to an early return, we set this to true.
        tombstoned: bool = false,
    };

    pub const Struct = struct {
        /// Analyte for tracking on the whole struct
        analyte: Analyte = .{},
        /// Field refinement indices - each field has its own refinement
        fields: []const EIdx,
    };

    scalar: Analyte,
    pointer: Indirected,
    optional: Indirected,
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": Struct, // struct with field refinements
    @"union": void, // unused, for now, temporarily void. Will be a slice EIdx.
    future: Future,
    noreturn: void, // specific return value for error paths.
    retval_future: void, // special-case for retvals before a return has been called.
    unimplemented: void, // this means we have an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.

    /// clobbers one refinement over another.  The dst structure must be identical.
    /// TODO: In some cases, "deeper specification" may be added.
    /// NOTE: For .future destinations, use clobber_structured_idx instead because
    /// clobber_future may reallocate dst_list, invalidating the dst pointer.
    pub fn clobber_structured(dst: *Refinement, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            .retval_future => @panic("cannot copy from .retval_future"),
            .region => try recurse_indirected(dst, dst_list, src, src_list, .region),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // future: use index-based version to avoid stale pointer after reallocation
            .future => @panic("clobber_structured: use clobber_structured_idx for .future destinations"),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    /// clobbers one refinement over another, using index for destination.
    /// Use this when dst might be .future (which requires reallocation-safe handling).
    pub fn clobber_structured_idx(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
        const dst = dst_list.at(dst_idx);
        switch (dst.*) {
            .scalar => {
                if (src != .scalar) std.debug.panic("clobber mismatch: src is .{s} and dst is .scalar", .{@tagName(src)});
                dst.scalar = src.scalar;
            },
            .pointer => try recurse_indirected(dst, dst_list, src, src_list, .pointer),
            .optional => try recurse_indirected(dst, dst_list, src, src_list, .optional),
            // retval_future is like future - a placeholder to be filled in
            .retval_future => try clobber_future(dst_idx, dst_list, src, src_list),
            .region => try recurse_indirected(dst, dst_list, src, src_list, .region),
            .@"struct" => recurse_fields(dst, dst_list, src, src_list, .@"struct"),
            .@"union" => recurse_fields(dst, dst_list, src, src_list, .@"union"),
            // future: use index-based clobber to handle reallocation
            .future => try clobber_future(dst_idx, dst_list, src, src_list),
            // following types don't have any metadata associated with them.
            .unimplemented => if (src != .unimplemented) std.debug.panic("clobber mismatch: src is .{s} and dst is .unimplemented", .{@tagName(src)}),
            .void => if (src != .void) std.debug.panic("clobber mismatch: src is .{s} and dst is .void", .{@tagName(src)}),
            .noreturn => if (src != .noreturn) std.debug.panic("clobber mismatch: src is .{s} and dst is .noreturn", .{@tagName(src)}),
        }
    }

    /// Clobber a .future slot with a src refinement, copying the structure without checks.
    /// Takes dst_idx instead of pointer because copy_to may reallocate dst_list.
    fn clobber_future(dst_idx: EIdx, dst_list: *Refinements, src: Refinement, src_list: *Refinements) error{OutOfMemory}!void {
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
            .region => |r| .{ .region = .{
                .analyte = r.analyte,
                .to = if (same_list) r.to else try copy_to(src_list.at(r.to).*, src_list, dst_list),
            } },
            .@"struct" => |s| blk: {
                if (same_list) {
                    break :blk src;
                }
                const allocator = dst_list.list.allocator;
                const new_fields = try allocator.alloc(EIdx, s.fields.len);
                for (s.fields, 0..) |field_idx, i| {
                    new_fields[i] = try copy_to(src_list.at(field_idx).*, src_list, dst_list);
                }
                break :blk .{ .@"struct" = .{ .analyte = s.analyte, .fields = new_fields } };
            },
            .@"union" => .{ .@"union" = {} },
            .unimplemented => .{ .unimplemented = {} },
            .void => .{ .void = {} },
            .noreturn => .{ .noreturn = {} },
            .future => |name| .{ .future = name }, // Preserve future (uninitialized slot)
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
            .future => |name| try dst_list.appendEntity(.{ .future = name }), // Copy future as-is (uninitialized slot)
            .region => try src.copy_to_indirected(src_list, dst_list, .region),
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
        if (tag == .@"union") {
            // Union not yet implemented
            return dst_list.appendEntity(.{ .unimplemented = {} });
        }
        // Copy struct fields
        const src_struct = src.@"struct";
        const allocator = dst_list.list.allocator;
        const new_fields = try allocator.alloc(EIdx, src_struct.fields.len);
        for (src_struct.fields, 0..) |field_idx, i| {
            new_fields[i] = try copy_to(src_list.at(field_idx).*, src_list, dst_list);
        }
        return dst_list.appendEntity(.{ .@"struct" = .{
            .analyte = src_struct.analyte,
            .fields = new_fields,
        } });
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
    // Free any struct field allocations
    for (self.list.items) |item| {
        if (item == .@"struct") {
            allocator.free(item.@"struct".fields);
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
/// Struct field slices are deep copied so each refinements list owns its own memory.
pub fn clone(self: *Refinements, allocator: Allocator) !Refinements {
    var new = Refinements.init(allocator);
    try new.list.ensureTotalCapacity(self.list.items.len);
    for (self.list.items) |item| {
        if (item == .@"struct") {
            // Deep copy struct fields
            const new_fields = try allocator.alloc(EIdx, item.@"struct".fields.len);
            @memcpy(new_fields, item.@"struct".fields);
            try new.list.append(.{ .@"struct" = .{
                .analyte = item.@"struct".analyte,
                .fields = new_fields,
            } });
        } else {
            try new.list.append(item);
        }
    }
    return new;
}

/// Mark all futures as tombstoned. Called on ret_safe to indicate
/// that this execution path exited and won't resolve its futures.
pub fn tombstoneAllFutures(self: *Refinements) void {
    for (self.list.items) |*item| {
        if (item.* == .future) {
            item.future.tombstoned = true;
        }
    }
}

const undefined_analysis = @import("analysis/undefined.zig");

pub fn testValid(self: *Refinements) void {
    undefined_analysis.testValid(self);
}