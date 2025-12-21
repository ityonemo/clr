const undefined_analysis = @import("analysis/undefined.zig");
const memory_safety_analysis = @import("analysis/memory_safety.zig");

/// Analyte holds the analysis state for a value.
/// Each analysis contributes its state type here.
undefined: ?undefined_analysis.Undefined = null,
memory_safety: ?memory_safety_analysis.MemorySafety = null,
