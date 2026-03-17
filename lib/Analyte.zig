const std = @import("std");
const undefined_safety_analysis = @import("analysis/undefined_safety.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");
const null_safety_analysis = @import("analysis/null_safety.zig");
const variant_safety_analysis = @import("analysis/variant_safety.zig");
const fieldparentptr_safety_analysis = @import("analysis/fieldparentptr_safety.zig");
const fd_safety_analysis = @import("analysis/fd_safety.zig");

const Analyte = @This();

/// Analyte holds the analysis state for a value.
/// Each analysis contributes its state type here.
undefined_safety: ?undefined_safety_analysis.UndefinedSafety = null,
memory_safety: ?memory_safety_analysis.MemorySafety = null,
null_safety: ?null_safety_analysis.NullSafety = null,
variant_safety: ?variant_safety_analysis.VariantSafety = null,
fieldparentptr_safety: ?fieldparentptr_safety_analysis.FieldParentPtrSafety = null,
fd_safety: ?fd_safety_analysis.FdSafety = null,

/// enumerated list of modes.  These two MUST be kept in sync.
pub const Mode = enum { undefined_safety, memory_safety, null_safety, variant_safety, fieldparentptr_safety, fd_safety };
pub const analyses =.{ 
    undefined_safety_analysis.UndefinedSafety, 
    memory_safety_analysis.MemorySafety, 
    null_safety_analysis.NullSafety, 
    variant_safety_analysis.VariantSafety, 
    fieldparentptr_safety_analysis.FieldParentPtrSafety, 
    fd_safety_analysis.FdSafety 
};

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
/// Calls .hash() on each analysis type that implements it.
pub fn hash(self: Analyte, hasher: *std.hash.Wyhash) void {
    inline for (@typeInfo(Analyte).@"struct".fields) |field| {
        if (@field(self, field.name)) |value| {
            hasher.update(&.{1}); // present
            if (@hasDecl(@TypeOf(value), "hash")) {
                value.hash(hasher);
            }
        } else {
            hasher.update(&.{0}); // absent
        }
    }
}

// =========================================================================
// Debug Formatting
// =========================================================================

/// Format undefined_safety state for debug output.
pub fn formatUndefined(self: Analyte) []const u8 {
    if (self.undefined_safety) |u| {
        return switch (u) {
            .undefined => "UNDEF",
            .defined => "defined",
            .inconsistent => "CONFLICT",
        };
    }
    return "null";
}

/// Format memory_safety state for debug output.
pub fn formatMemSafety(self: Analyte) []const u8 {
    if (self.memory_safety) |m| {
        return switch (m) {
            .allocated => |a| if (a.freed != null) "freed" else "allocated",
            .stack => "stack",
            .global => "global",
            .unset => "unset",
            .error_stub => "error_stub",
        };
    }
    return "null";
}
