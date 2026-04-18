---
paths:
  - "lib/analysis/fieldparentptr_safety.zig"
---

# FieldParentPtr Safety Analysis

## Refinement Types with fieldparentptr_safety

**ALLOWED (may be null or non-null)**:
- `pointer` - tracks where the pointer came from (struct field origin)

**MUST BE NULL (has analyte, but fieldparentptr_safety invalid)**:
- `scalar`, `optional`, `errorunion`, `struct`, `union`, `recursive`, `fnptr`, `allocator`, `region`

**NO ANALYTE (no analyte struct exists)**:
- `void`, `noreturn`, `unimplemented`

NOTE: init() functions may leave these invariants temporarily unsatisfied, as long as the tag handler sets the invariants correctly before the tag processing is complete.

## Purpose

Validates `@fieldParentPtr` operations by tracking where pointers came from.

## State Structure

`FieldParentPtrSafety` tracks:
- `field_index` - which field of the container the pointer came from
- `container_type_id` - type id for type identity and name lookup

## How It Works

1. `struct_field_ptr` records the container type and field index on the resulting pointer
2. When `@fieldParentPtr` is called, validate that:
   - The pointer actually came from a struct field
   - The container type matches the expected type
   - The field index matches the expected field
