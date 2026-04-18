---
paths:
  - "lib/analysis/memory_safety.zig"
---

# Memory Safety Analysis

## Refinement Types with memory_safety

NOTE: init() functions may leave these invariants temporarily unsatisfied, as long as the tag handler sets
the invariants correctly before the tag processing is complete. 

**MUST EXIST (must be non-null)**:
- `scalar`, `pointer`, `optional`, `errorunion`, `region`, `recursive`, `struct`, `union`, `fnptr`, `allocator`

**NO ANALYTE (no analyte struct exists)**:
- `void`, `noreturn`, `unimplemented`

## Purpose

Tracks memory provenance and allocation state:
- **Stack** - data lives on the stack
- **Allocated** - data lives on the heap (via allocator)
- **Interned** - compile-time/interned data (globals, string literals)

Detects: memory leaks, use-after-free, double-free, allocator mismatches, stack escapes.

## Key Principle: Spatial Memory

`memory_safety` describes where a refinement's memory **spatially** lives. It does NOT traverse pointer `.to` fields because `.to` references separate memory.  Other refinement types do have unified `.to` spatial memory.

- For a pointer: `pointer.analyte.memory_safety` = where the pointer VALUE lives
- The pointee's location is found by following `pointer.to` and checking THAT refinement's `memory_safety`

## Setting Memory Safety

Use `paintSpatialMemory()` to set memory_safety on a refinement and its nested components (struct fields, etc.). This function does NOT traverse pointer `.to` fields.

- Called when allocating (set `.allocated` on the allocated region/element)
- Called when creating stack values (set `.stack`)
- Called when loading interned values (set `.interned`)

## NO other painting functions.

No functions should directly paint spatial memory EXCEPT for paintSpatialMemory, unless you're calling paintSpatialMemory.

## NO side-channel recursive painting of pointer types.

There should be no side-channel recursive assignment of pointer types' to field, with the following exceptions:

- cloning the refinements table.
- initializing a new, unassigned pointer and setting it to .placeholder.
- allocator activity that sets the pointed-to areas as .allocated (but this should still go via paintSpatialMemory)

## NO checking other analyte types.

It is tempting to check the .undefined analyte type.  Don't do it.

## init() strategy.

init() should be null for runtime spatial memory, and setting memory_safety on those
spatial memory zones should be the responsibility of the tag handler.

## Allocation Tracking

Allocations are tracked on the **data** being allocated, not the pointer to it:
- `allocator.create(T)` → set `.allocated` on the element (scalar/struct)
- `allocator.alloc(T, n)` → set `.allocated` on the region

The pointer returned by these operations may live on the stack (as a return value), but points to heap data.

## Leak Detection

At branch merge and function end, check for orphaned allocations:
- Allocations created in a branch that aren't reachable from pre-branch entities
- Allocations that weren't freed or returned before function exit

Use `hasMatchingAllocation()` to search for allocations reachable from a given refinement by following `.to` and struct fields.
