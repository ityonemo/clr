# CLR Fixes Log

This document records bugs found and fixed during vendor wrapper testing, including root cause analysis and fixes applied.

---

## Fix: Union fieldParentPtr provenance through interned and global union fields

**Date:** 2026-05-21

**Symptom:**
Union `fieldParentPtr` cases still failed after core union-state bringup,
especially parent recovery through union fields, tagged union fields, and global
union fields.

**Root Cause:**
Comptime-known union literals did not carry active-field metadata into generated
analyzers, so stores from interned union values could create a union structure
without selecting the active variant. Whole-union `undefined` checks also fired
too early on tag checks, blocking valid field assignment patterns. Finally,
`field_parent_ptr` depended too heavily on the field pointer's direct memory
root, which was missing or stale for interned/global field pointers.

**The Fix:**
Generated `Interned.active_union_field` for known union literals, used it during
stores to initialize the destination union's active field, and moved
whole-union undefined reporting to actual field-value reads. `field_parent_ptr`
now paints the parent pointer's pointee shape and can recover the parent by
field-origin metadata when the immediate memory root is unavailable.

**Key Files Changed:**
- `src/codegen.zig`
- `lib/core.zig`
- `lib/tag.zig`
- `lib/analysis/variant_safety.zig`
- `lib/analysis/undefined_safety.zig`
- `lib/analysis/memory_safety.zig`

**Test Cases:**
- `test/cases/fieldparentptr_safety/valid_union_field.zig`
- `test/cases/fieldparentptr_safety/globals/valid_union_field.zig`
- `test/cases/allocator_safety/field_ptr/free_via_fieldparentptr_union.zig`
- `test/cases/allocator_safety/field_ptr/free_via_fieldparentptr_tagged_union.zig`

---

## Fix: Core union state initialization and whole-union undefined reporting

**Date:** 2026-05-21

**Symptom:**
Union cases could panic in `testValid` with missing `variant_safety`, and whole
unions assigned `undefined` either passed silently or failed before producing the
intended undefined-value diagnostic.

**Root Cause:**
Union refinements were structurally created with nullable field slots but no
default variant-state analyte. That conflicted with the strict invariant that
all unions must carry `variant_safety`. Whole-union undefined stores also had no
place to preserve the source location because union containers intentionally do
not carry `undefined_safety`.

**The Fix:**
Initialized every union with an all-inactive `variant_safety` state, added
`union_init` handling to mark the initialized variant active, and recorded
whole-union undefined source metadata in `variant_safety`. Undefined safety now
reports `UseBeforeAssign` when a whole-undefined union reaches tag-check or
field-access paths, before variant narrowing can manufacture an active field.

Also initialized pointer-field targets created by union variant setup as memory
placeholders, so recursive union pointer fields satisfy memory-safety validation.

**Key Files Changed:**
- `lib/analysis/variant_safety.zig`
- `lib/analysis/undefined_safety.zig`
- `lib/analysis/memory_safety.zig`

**Test Cases:**
- `undefined_safety/unions/tagged_whole_undefined.zig`
- `undefined_safety/unions/untagged_whole_undefined.zig`
- `undefined_safety/globals/use_undefined_union.zig`
- `allocator_safety/recursive/expr_tree_correct.zig`
- `stack_pointer_safety/union/no_escape.zig`

---

## Fix: ptr_slice_ptr_ptr creates fresh pointer chain instead of pointing to original slice

**Date:** 2026-04-15

**Symptom:**
"memory leak detected at if/else merge" in `ArrayList.ensureTotalCapacityPrecise` when allocating new memory and storing it via `self.items.ptr = new_alloc.ptr`.

**Root Cause:**
The `SliceFieldPtr.apply` handler (used by `ptr_slice_ptr_ptr`) created a fresh pointer chain from the type description instead of creating a pointer that points to the original slice entity. When storing through this fresh pointer, the store updated the fresh chain, not the original struct field. At branch merge, the allocation was not seen as reachable from the original struct.

**When This Happens:**
- Container struct has a slice field (e.g., `items: []u8`)
- Code stores allocation to slice's ptr field: `container.items.ptr = new_alloc.ptr`
- This generates: `struct_field_ptr` → `ptr_slice_ptr_ptr` → `slice_ptr` → `store`
- The `ptr_slice_ptr_ptr` result was a fresh pointer, not connected to the original `items` field
- At branch merge, orphan detection doesn't find allocation reachable from container

**The Fix:**
Modified `SliceFieldPtr.apply` to:
1. Get the source pointer's refinement
2. Get the source pointer's `.to` (the slice GID)
3. If the slice is a pointer, create result pointer with `.to = slice_gid`
4. This ensures stores through the result update the original slice

Now when we store through `ptr_slice_ptr_ptr`, the store handler updates `slice.pointer.to = src.pointer.to`, connecting the original struct field to the new allocation.

**Additional Fixes:**
Added guards in `memory_safety.onFinish` and `memory_safety.cond_br` to skip refinements without analytes (void, noreturn, unimplemented, fnptr), which could cause panics when checking pointer pointees.

**Key Files Changed:**
- `lib/tag.zig` - Modified `SliceFieldPtr.apply` to point to original slice
- `lib/analysis/memory_safety.zig` - Added void/fnptr guards in `onFinish` and `cond_br`

**Test Case:** `test/cases/misc/slice_ptr_store_reachable.zig` (test "no false positive for allocation stored through slice ptr field")

---

## Fix: Create/free method mismatch not detected after bitcast

**Date:** 2026-03-31

**Symptom:**
When allocating with `create(u8)` and freeing with `free(slice)` after a bitcast from `*u8` to `*[1]u8`, the analyzer reported "memory leak" instead of "allocation method mismatch".

**Root Cause:**
The `is_slice` check in `handleAllocDestroy` and `handleAllocFree` derived the allocation method from the current refinement structure (`pointee.* == .region` = slice). When bitcast converted `*u8` (pointer → scalar) to `*[1]u8` (pointer → region → scalar), the structure changed, making the analyzer think a single-item `create` allocation was now a slice allocation.

**When This Happens:**
- Allocate with `allocator.create(u8)` → returns `*u8`
- Cast to `*[1]u8` via `@ptrCast` → bitcast wraps scalar in region
- Convert to slice via array coercion → `[]u8`
- Free with `allocator.free(slice)` → analyzer sees region, thinks it's slice allocation

**Key Insight:**
The allocation method is determined by the TYPE of allocation:
- `create` ALWAYS returns pointer → scalar/struct (single item)
- `alloc` ALWAYS returns pointer → region → element (slice)

Therefore, WHERE the `.allocated` metadata lives tells us the allocation method:
- `.allocated` on region → real `alloc` allocation
- `.allocated` on element (scalar/struct) with no `.allocated` on region → `create` allocation (possibly wrapped by bitcast)

**The Fix:**
1. When bitcast wraps a scalar in a region (converting `*T` to `[*]T`), the region is created WITHOUT allocation metadata
2. The original scalar keeps its `.allocated` metadata from `create`
3. In `handleAllocFree`, detect mismatch by checking structure:
   - If region has `.allocated` → it's a real `alloc` allocation → OK for `free`
   - If region has NO `.allocated` but element does → it's a `create` allocation wrapped by bitcast → MISMATCH
4. Same logic in `handleAllocDestroy` for the reverse case

This approach requires no additional fields - the refinement structure itself encodes the allocation method.

**Key Files Changed:**
- `lib/analysis/memory_safety.zig` - Updated `handleAllocFree` and `handleAllocDestroy` to detect mismatch via structure
- `lib/tag.zig` - Ensure bitcast does NOT copy allocation metadata to new region wrapper

**Test Case:** `test/cases/allocator_safety/basic/create_free_mismatch.zig` (test #25)

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

## Fix: slice_ptr must produce pointer to region (multi-item pointer)

**Date:** 2026-03-30

**Symptom:**
When running code that does pointer arithmetic on `slice.ptr` (e.g., `slice.ptr + 1`), the analysis incorrectly reported an error about pointer arithmetic on a single-item pointer.

**Root Cause:**
The `slice_ptr` handler was creating a pointer to a scalar instead of a pointer to a region. In Zig's type system:
- `*T` (single-item pointer) → pointer to scalar
- `[*]T` (multi-item pointer) → pointer to region

A slice is `[]T` which is represented as `pointer → region → element`. When you access `slice.ptr`, you get a `[*]T` (multi-item pointer), which should be `pointer → region`.

**When This Happens:**
When code extracts the `.ptr` from a slice and performs pointer arithmetic:
```zig
const slice: []const u8 = &data;
const ptr = slice.ptr;     // Should be [*]const u8 (multi-item)
const second = ptr + 1;    // ptr_add - only valid on multi-item pointers
```

**The Fix:**
The `slice_ptr` handler correctly creates a new pointer that points to the same region as the slice, not to the element. This was verified to be working correctly, but was missing a unit test.

**Unit Test:** `tag.test.slice_ptr produces pointer to region (multi-item pointer)` in `lib/tag.zig`
**Integration Test:** `test/cases/memory/slice_ptr_add.zig`

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

## Fix: Bitcast from [*]u8 to [*]Struct must convert region element type

**Date:** 2026-03-30

**Symptom:**
When running the HashMap vendor wrapper, false positive "use of undefined value" error occurred in `Metadata.isUsed` function even though the metadata had been initialized via `@memset`.

**Root Cause:**
HashMap allocates memory using `alignedAlloc(u8, ...)` which returns `[*]u8`, then bitcasts to `[*]Metadata`. The Bitcast handler was sharing the existing refinement when bitcasting between region pointers, which meant the region element type stayed as scalar instead of being converted to struct.

When `@memset` was called on the region, it marked the scalar element as defined. But when code accessed `.used` field on a metadata element, the analysis looked for struct fields on what was still a scalar refinement, found nothing, and incorrectly reported an undefined value.

**When This Happens:**
When code allocates raw bytes and bitcasts to a structured type:
```zig
const slice = allocator.alignedAlloc(u8, @alignOf(Metadata), size);
const metadata: [*]Metadata = @ptrCast(slice.ptr);
@memset(metadata[0..count], .{ .fingerprint = 0, .used = false });
if (metadata[0].used) { ... }  // False positive: "use of undefined value"
```

**The Fix:**
Modified the `Bitcast.apply` handler in `lib/tag.zig` to detect when bitcasting a pointer to region of scalars to a pointer to region of structs. When this happens:
1. Create a new struct element refinement with the correct field structure
2. Copy the undefined state from the source scalar to all struct fields
3. Create a new region pointing to the new struct element
4. Create a new pointer pointing to the new region

Added helper function `copyUndefinedStateFromScalarToStruct` to propagate undefined state from scalar to struct fields recursively.

**Unit Test:** `tag.test.bitcast converts [*]u8 to [*]Struct region element type` in `lib/tag.zig`
**Integration Test:** `test/cases/misc/bitcast_scalar_to_struct.zig`, `test/integration/misc.bats`

---

## Fix: Clear source allocation tracking when bitcast creates new region

**Date:** 2026-03-30

**Symptom:**
False positive memory leak reported when using HashMap-style pointer arithmetic pattern. Allocating `[Header][Data]` as bytes, storing a derived pointer (`ptr + sizeof(Header)`) to an optional field, then freeing via reverse arithmetic (`ptr - sizeof(Header)`) incorrectly reported a memory leak.

**Root Cause:**
When bitcast converts `[*]u8` to `?[*]Metadata` (optional pointer to region of structs), it creates NEW refinement entities (pointer → region → struct) with memory_safety.allocated copied from the source. However, the SOURCE region still had memory_safety.allocated with freed=null.

The leak detector found two regions with allocated state:
1. Source region (from original slice_ptr/ptr_add) with allocated, freed=null
2. New region (from bitcast type conversion) with allocated, freed=null

Only the new region's pointer was stored to self.metadata. When the allocate function returned, the source region was detected as a leak because it had allocated state but wasn't stored anywhere.

**When This Happens:**
When code stores a bitcast'd pointer that required type conversion:
```zig
const slice = allocator.alloc(u8, total_size);
const ptr: [*]u8 = slice.ptr;
const data_ptr = ptr + @sizeOf(Header);  // Derived pointer
self.metadata = @ptrCast(@alignCast(data_ptr));  // Bitcast to ?[*]Struct
// Source region (data_ptr) still has allocated state → false positive leak
```

**The Fix:**
After creating the new region with copied memory_safety during bitcast type conversion, clear the SOURCE region's memory_safety (set to null). The new region now owns the allocation tracking, so the source should not be tracked. This uses connectivity tracking - the allocation is only tracked via the new region which is reachable through self.metadata.

Added helper function `clearAllocationTracking` in `lib/tag.zig` that sets memory_safety to null on a region.

Also fixed `copyAnalyteStateFromScalarToStruct` to only copy memory_safety (not undefined_safety) to struct containers, since structs cannot have undefined_safety at the container level.

**Unit Tests:** 
- `tag.test.ptr_sub on pointer to region returns pointer to same region`
- `tag.test.ptr_add on pointer to region returns pointer to same region`
- `tag.test.bitcast from [*]T to *T preserves region structure`

**Integration Tests:** 
- `test/cases/memory/optional_ptr_arithmetic_free.zig`
- `test/cases/memory/ptr_arithmetic_free.zig`
- `test/integration/allocator_slice.bats`

---
