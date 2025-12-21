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
    unset_retval: void, // special-case for retval slots before a return has been called.
    region: Indirected, // unused, for now, will represent slices (maybe)
    @"struct": void, // unused, for now, temporarily void. Will be a slice of EIdx.
    @"union": void, // unused, for now, temporarily void. Will be a slice EIdx.
    unimplemented: void, // this means we have an operation that is unimplemented but does carry a value.
    void: void, // any instructions that don't store anything.
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

/// Allocate a new unset_retval entity and return its index.
/// Used for pre-allocating return value entities in caller's refinements.
pub fn initEntity(self: *Refinements) !EIdx {
    const idx: EIdx = @intCast(self.list.items.len);
    try self.list.append(.unset_retval);
    return idx;
}

/// Append a new entity with the given value and return its index.
pub fn appendEntity(self: *Refinements, value: Refinement) !EIdx {
    const idx: EIdx = @intCast(self.list.items.len);
    try self.list.append(value);
    return idx;
}

/// Recursively copy a Refinement from another Refinements table into an existing slot in this one.
/// Handles EIdx references by recursively copying referenced entities.
pub fn copyInto(self: *Refinements, dest_idx: EIdx, src_refinements: *Refinements, src_idx: EIdx) !void {
    const src = src_refinements.list.items[src_idx];

    // IMPORTANT: We must compute the new value BEFORE assigning to self.list.items[dest_idx]
    // because copyEntityRecursive may append to self.list, which could reallocate the backing
    // array and invalidate any pointers/slices we're holding.
    const copied: Refinement = switch (src) {
        .pointer => |ind| blk: {
            const new_to = try self.copyEntityRecursive(src_refinements, ind.to);
            break :blk .{ .pointer = .{ .analyte = ind.analyte, .to = new_to } };
        },
        .optional => |ind| blk: {
            const new_to = try self.copyEntityRecursive(src_refinements, ind.to);
            break :blk .{ .optional = .{ .analyte = ind.analyte, .to = new_to } };
        },
        .region => |ind| blk: {
            const new_to = try self.copyEntityRecursive(src_refinements, ind.to);
            break :blk .{ .region = .{ .analyte = ind.analyte, .to = new_to } };
        },
        // These don't contain EIdx references, copy directly
        .scalar, .unset_retval, .@"struct", .@"union", .unimplemented, .void => src,
    };
    self.list.items[dest_idx] = copied;
}

/// Recursively copy a Refinement, appending to this table. Returns new entity index.
// TODO: Make this non-recursive to avoid stack overflow with deeply nested structures.
pub fn copyEntityRecursive(self: *Refinements, src_refinements: *Refinements, src_idx: EIdx) !EIdx {
    const src = src_refinements.list.items[src_idx];
    // First recursively copy any referenced entities, then append this entity
    const copied: Refinement = switch (src) {
        .pointer => |ind| .{ .pointer = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_refinements, ind.to) } },
        .optional => |ind| .{ .optional = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_refinements, ind.to) } },
        .region => |ind| .{ .region = .{ .analyte = ind.analyte, .to = try self.copyEntityRecursive(src_refinements, ind.to) } },
        .scalar, .unset_retval, .@"struct", .@"union", .unimplemented, .void => src,
    };
    // Compute idx AFTER recursive calls, since they may have appended entities
    const idx: EIdx = @intCast(self.list.items.len);
    try self.list.append(copied);
    return idx;
}
