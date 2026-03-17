# CLR Fixes Log

This document records bugs found and fixed during vendor wrapper testing, including root cause analysis and fixes applied.

---

## Fix: clearAllocationMetadata must use .unset instead of null

**Date:** 2026-03-17

**Commit:** a517cf4

**Symptom:**
When running the vendor wrapper, panic occurred: `memory_safety must be set on scalars (gid=1370)` in `testValid`.

**Root Cause:**
The `clearAllocationMetadata` function in `lib/analysis/memory_safety.zig` was setting `memory_safety = null` on refinements when clearing phantom allocation metadata on error paths.

**When This Happens:**
When an error union's payload contains an allocation (like `allocator.create(u8)` returning `!*u8`), and the error path is taken, the analysis needs to clear the "phantom" allocation metadata from the payload - because on the error path, the allocation never actually happened.

**The Problem:**
The `testValid` invariant requires ALL scalars to have `memory_safety` set (not null). By setting `memory_safety = null`, `clearAllocationMetadata` violated this invariant. When the error path merged back and `testValid` was called, it found scalars with `memory_safety = null` and panicked.

**The Fix:**
Changed `clearAllocationMetadata` to set `memory_safety = .{ .unset = {} }` instead of `null`. The `.unset` variant means "no active allocation tracking" while still satisfying the invariant that memory_safety must be set.

For error unions specifically, we use `.error_stub` instead of `.unset` since that's the expected state for error union containers.

**Code Change (line 643 example):**
```zig
// Before (wrong):
.scalar => |*s| s.analyte.memory_safety = null,

// After (correct):
.scalar => |*s| s.analyte.memory_safety = .{ .unset = {} },
```

**Test Case:** `test/cases/misc/error_path_clear_metadata.zig`

---

## Fix: ArrayElemVal must handle interned sources

**Date:** 2026-03-17

**Commit:** 53e4fc6

**Symptom:**
When running the GPA error path test, panic occurred: `interned source not implemented` in ArrayElemVal tag handler.

**Root Cause:**
The `ArrayElemVal` tag handler in `lib/tag.zig` only handled `.inst` sources (AIR instruction references) but not `.interned` sources (comptime-known values from the InternPool).

**When This Happens:**
When code iterates over comptime-known arrays. GPA's `detectLeaks` function iterates over bucket arrays that are comptime-known, generating `array_elem_val` instructions with interned sources.

**The Problem:**
The handler would panic on any `.interned` source because it was marked as unimplemented. For comptime arrays, we need to either find an existing global refinement for the interned value, or create a new refinement based on the element type.

**The Fix:**
Added handling for `.interned` sources in ArrayElemVal:
1. First try to look up an existing global refinement via `refinements.getGlobal(interned.ip_idx)`
2. If not found, extract the element type from the interned type info
3. Create a new refinement using `typeToRefinement` for the element type
4. Mark it as defined via `splatInitDefined` (comptime values are always defined)

**Code Change:**
```zig
.interned => |interned| blk: {
    // Try to find existing global first
    if (state.refinements.getGlobal(interned.ip_idx)) |gid| {
        break :blk gid;
    }
    // Create element from type for comptime arrays
    const element_type = switch (interned.ty) {
        .region => |elem| elem.*,
        .pointer => |inner| switch (inner.*) {
            .region => |elem| elem.*,
            else => @panic("ArrayElemVal: interned pointer must point to region"),
        },
        else => @panic("ArrayElemVal: interned type must be region or pointer to region"),
    };
    const element_ref = try typeToRefinement(element_type, state.refinements);
    const element_gid = try state.refinements.appendEntity(element_ref);
    splatInitDefined(state.refinements, element_gid, state.ctx);
    state.results[index].refinement = element_gid;
    try splat(.array_elem_val, state, index, self);
    return;
},
```

**Test Case:** `test/cases/misc/gpa_error_path.zig`

---
