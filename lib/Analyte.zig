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
