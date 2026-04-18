---
paths:
  - "lib/analysis/variant_safety.zig"
---

# Variant Safety Analysis

## Refinement Types with variant_safety

**MUST EXIST (must be non-null)**:
- `union` - tracks which variants are active; each variant field tracks its own undefined state separately

**MUST BE NULL (has analyte, but variant_safety invalid)**:
- `scalar`, `pointer`, `optional`, `errorunion`, `struct`, `recursive`, `fnptr`, `allocator`, `region`

**NO ANALYTE (no analyte struct exists)**:
- `void`, `noreturn`, `unimplemented`

NOTE: init() functions may leave these invariants temporarily unsatisfied, as long as the tag handler sets the invariants correctly before the tag processing is complete.

## Purpose

Tracks which union variants are active. Detects access to inactive union variants.

## State Structure

`VariantSafety` has `active_metas: []?Meta` - one entry per union field:
- `null` - field is inactive
- `Meta` - field is active (with location where it was set)

## Key Operations

- `set_union_tag` - activates one variant and deactivates others
- Field access checks if the accessed variant is active
- At branch merge, reconcile active variants from both branches

## Validation

The `testValid` function ensures `variant_safety` only exists on `.union` refinements. Other refinement types should never have `variant_safety` set.
