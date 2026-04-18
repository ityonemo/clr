---
paths:
  - "lib/analysis/undefined_safety.zig"
---

# Undefined Safety Analysis

## Refinement Types with undefined_safety

**MUST EXIST (must be non-null - testValid panics if null)**:
- `scalar` - tracks if the value is defined
- `pointer` - tracks if the pointer value is defined
- `fnptr` - tracks if the function pointer is defined
- `allocator` - tracks if the allocator value is defined

**MUST BE NULL (container types - track on children instead)**:
- `optional` - track on payload via `.to`
- `errorunion` - track on payload via `.to`
- `struct` - track on each field
- `region` - track on element type via `.to`
- `union` - variant_safety tracks active variant; each variant field tracks its own undefined state
- `recursive` - placeholder for recursive type references

**NO ANALYTE (no analyte struct exists)**:
- `void`, `noreturn`, `unimplemented`

NOTE: init() functions may leave these invariants temporarily unsatisfied, as long as the tag handler sets the invariants correctly before the tag processing is complete.

## Purpose

Tracks whether values are defined or undefined. Detects use-before-assign errors.

## States

- **defined** - value has been assigned
- **undefined** - value declared but not assigned (e.g., `var x: u8 = undefined`)
- **inconsistent** - value is defined in some branches but undefined in others

## Key Rules

1. For `.optional`, `.errorunion`, and `.struct` refinements, the top-level `analyte.undefined_safety` should always be null. Track undefined state on the payload/fields, not the container.

2. When a value is stored, mark the destination as defined.

3. When a value is loaded/used, check if it's undefined and report error if so.

4. At branch merge, if a value is defined in one branch but undefined in another, mark it as `.inconsistent`.

5. `setSafetyState` does NOT traverse `pointer.to` - pointees are separate memory. The pointer value's undefined state is independent from what it points to.

## Container Types

- **Structs**: Track undefined state per-field, not on the struct itself
- **Optionals**: Track on the payload
- **Error unions**: Track on the payload
