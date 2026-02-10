const std = @import("std");
const undefined_analysis = @import("analysis/undefined_safety.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");
const null_safety_analysis = @import("analysis/null_safety.zig");
const variant_safety_analysis = @import("analysis/variant_safety.zig");
const fieldparentptr_safety_analysis = @import("analysis/fieldparentptr_safety.zig");

const Analyte = @This();

/// Analyte holds the analysis state for a value.
/// Each analysis contributes its state type here.
undefined_safety: ?undefined_analysis.UndefinedSafety = null,
memory_safety: ?memory_safety_analysis.MemorySafety = null,
null_safety: ?null_safety_analysis.NullSafety = null,
variant_safety: ?variant_safety_analysis.VariantSafety = null,
fieldparentptr_safety: ?fieldparentptr_safety_analysis.FieldParentPtrSafety = null,

/// Deep copy the analyte, calling .copy() on each analysis type.
pub fn copy(self: Analyte, allocator: std.mem.Allocator) error{OutOfMemory}!Analyte {
    var result: Analyte = .{};
    inline for (@typeInfo(Analyte).@"struct".fields) |field| {
        if (@field(self, field.name)) |value| {
            @field(result, field.name) = try value.copy(allocator);
        }
    }
    return result;
}

/// Free allocated resources in analysis types (e.g., variant_safety.active_metas).
pub fn deinit(self: Analyte, allocator: std.mem.Allocator) void {
    inline for (@typeInfo(Analyte).@"struct".fields) |field| {
        if (@field(self, field.name)) |value| {
            if (@hasDecl(@TypeOf(value), "deinit")) {
                value.deinit(allocator);
            }
        }
    }
}

/// Hash the analyte state for memoization.
pub fn hash(self: Analyte, hasher: *std.hash.Wyhash) void {
    // Hash undefined_safety
    if (self.undefined_safety) |us| {
        hasher.update(&.{1});
        hasher.update(&.{@intFromEnum(us)});
    } else {
        hasher.update(&.{0});
    }

    // Hash memory_safety
    if (self.memory_safety) |ms| {
        hasher.update(&.{1});
        hasher.update(&.{@intFromEnum(ms)});
        switch (ms) {
            .allocated => |a| {
                hasher.update(&.{@as(u8, if (a.freed != null) 1 else 0)});
                hasher.update(&.{@as(u8, if (a.returned) 1 else 0)});
            },
            .stack, .global, .unset, .error_stub => {},
        }
    } else {
        hasher.update(&.{0});
    }

    // Hash null_safety
    if (self.null_safety) |ns| {
        hasher.update(&.{1});
        hasher.update(&.{@intFromEnum(ns)});
    } else {
        hasher.update(&.{0});
    }

    // Hash variant_safety
    if (self.variant_safety) |vs| {
        hasher.update(&.{1});
        for (vs.active_metas) |am| {
            hasher.update(&.{@as(u8, if (am != null) 1 else 0)});
        }
    } else {
        hasher.update(&.{0});
    }

    // Hash fieldparentptr_safety
    if (self.fieldparentptr_safety) |fps| {
        hasher.update(&.{1});
        hasher.update(std.mem.asBytes(&fps.field_index));
        hasher.update(std.mem.asBytes(&fps.container_type_id));
    } else {
        hasher.update(&.{0});
    }
}
