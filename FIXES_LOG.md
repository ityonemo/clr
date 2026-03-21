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

## Fix: Store handler must call splat BEFORE structural updates

**Date:** 2026-03-18

**Commit:** 6f5650a

**Symptom:**
False positive "use of undefined value" errors when storing a struct value into a struct field, specifically when using `return .{ .inner = inner }` patterns. Observed with `std.fs.File.deprecatedReader()` which stores a File struct into a GenericReader.context field.

**Root Cause:**
The `Store.apply` handler in `lib/tag.zig` was modifying `ptr_ref.pointer.to` to point to the source struct BEFORE calling `splat(.store)`. When `undefined_safety.store` ran, it would read the already-modified pointer and copy undefined state from source to source (a no-op), instead of from source to destination.

**When This Happens:**
When storing a struct value into a struct field via ret_ptr/struct_field_ptr pattern:
1. `ret_ptr` creates a struct (e.g., GenericReader) with fields marked as undefined
2. `struct_field_ptr` gets pointer to a field (e.g., GenericReader.context, a File struct)
3. `store` should copy undefined state from arg (defined File) to the field (undefined File)

**The Problem:**
In `Store.apply`:
```zig
// Store tag handler modified ptr.to HERE (line 1262)
if (src.* == .@"struct") {
    ptr_ref.pointer.to = src_ref;  // Now points to SOURCE
}
// Then called splat (line 1275)
try splat(.store, state, index, self);  // undefined_safety reads modified ptr.to
```

When `undefined_safety.store` ran:
- It read `ptr_ref.pointer.to` which was now the source GID
- Called `copyUndefinedStateRecursive(dst=source, src=source)` - a no-op!
- The original destination (inside GenericReader) retained its undefined state

**The Fix:**
Call `splat(.store)` BEFORE the structural updates:
```zig
// Call splat FIRST (analyses see original destination)
if (!is_allocator) {
    try splat(.store, state, index, self);
}

// THEN update structural fields
if (src.* == .@"struct") {
    ptr_ref.pointer.to = src_ref;
}
```

Special case for allocator: the allocator path calls splat after because it uses a different update pattern (direct reference).

**Test Case:** `test/cases/store_struct_field_preserves_state.zig`

---

## Fix: Allocator struct fields must emit .allocator type, not .@"struct"

**Date:** 2026-03-21

**Symptom:**
When running the ForestMQ vendor wrapper (or any code storing an allocator in a struct field), panic occurred: `access of union field 'struct' while field 'allocator' is active` in `copyUndefinedState`.

**Root Cause:**
In `src/codegen.zig`, the `typeToStringLookupNoNames` and `typeToStringLookup` functions generate type representations for struct fields. When a field's type is `std.mem.Allocator`, these functions were generating `.@"struct"` instead of `.allocator`.

**When This Happens:**
When a struct contains an `std.mem.Allocator` field (a very common Zig pattern):
```zig
const Container = struct {
    allocator: std.mem.Allocator,
    data: u32,
};
```

The codegen would emit:
```zig
.{ .@"struct" = &.{ .type_id = X, .fields = &.{ .{ .@"struct" = ... }, .{ .scalar = {} } } } }
//                                                   ^^^^^^^^^ WRONG
```

Instead of:
```zig
.{ .@"struct" = &.{ .type_id = X, .fields = &.{ .{ .allocator = Y }, .{ .scalar = {} } } } }
//                                                   ^^^^^^^^^ CORRECT
```

**The Problem:**
The `.struct_type` handling in both `typeToStringLookupNoNames` and `typeToStringLookup` immediately delegated to `structTypeToStringSimple` / `structTypeToString` without first checking if the struct type is the well-known `std.mem.Allocator` type.

The `isAllocatorType` function existed and correctly identified allocator types by checking if the struct name is `"mem.Allocator"`, but it wasn't being called in the struct type conversion path.

**The Fix:**
Added `isAllocatorType(ip, ty)` check at the beginning of both `.struct_type` cases:

```zig
.struct_type => blk: {
    // Check for well-known struct type: std.mem.Allocator
    if (isAllocatorType(ip, ty)) {
        break :blk formatAllocatorType(arena, @intFromEnum(ty));
    }
    // ... existing struct handling
}
```

Also added a testable helper `formatAllocatorType()` that generates the allocator type string.

**Files Changed:**
- `src/codegen.zig` - Added `formatAllocatorType()` helper, added `isAllocatorType` checks in both type conversion functions
- `src/codegen_test.zig` - Added test for `formatAllocatorType`, updated `dbg_var_val` test to expect noop
- `lib/tag.zig` - Removed obsolete `dbg_var_val` test, updated comments
- `lib/Inst.zig`, `lib/Context.zig` - Updated comments about dbg_var_val

**Test Case:** `test/cases/misc/allocator_in_struct.zig`

---

## Fix: struct_field_val must handle region fields

**Date:** 2026-03-21

**Symptom:**
When running the ForestMQ vendor wrapper, panic occurred: `struct_field_val: unexpected field refinement type region` in `checkFieldUndefined`.

**Root Cause:**
The `checkFieldUndefined` function in `lib/analysis/undefined_safety.zig` handles undefined checking for struct field values. It handled `.scalar`, `.pointer`, `.optional`, `.@"struct"`, and `.@"union"` types, but not `.region` (array/slice) types.

**When This Happens:**
When a struct contains an array field (like `buf: [32]u8`), extracting that field generates a `struct_field_val` instruction with a `.region` type. The Wyhash struct in stdlib has such array fields.

**The Fix:**
Added `.region` to the list of container types that don't track undefined on themselves (their elements carry the undefined state):

```zig
// Before:
.optional, .@"struct", .@"union" => {},

// After:
.optional, .@"struct", .@"union", .region => {},
```

**Test Case:** `test/cases/misc/struct_with_array_field.zig`

---
