const undefined_analysis = @import("analysis/undefined.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");
const null_safety_analysis = @import("analysis/null_safety.zig");
const variant_safety_analysis = @import("analysis/variant_safety.zig");
const fieldparentptr_safety_analysis = @import("analysis/fieldparentptr_safety.zig");

/// Analyte holds the analysis state for a value.
/// Each analysis contributes its state type here.
undefined: ?undefined_analysis.Undefined = null,
memory_safety: ?memory_safety_analysis.MemorySafety = null,
null_safety: ?null_safety_analysis.NullSafety = null,
variant_safety: ?variant_safety_analysis.VariantSafety = null,
fieldparentptr_safety: ?fieldparentptr_safety_analysis.FieldParentPtrSafety = null,
