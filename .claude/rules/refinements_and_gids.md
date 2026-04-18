---
paths:
  - "lib/*.zig"
---

# Refinements and GIDs

## Overview

A `Refinements` table is a list of `Refinement` entities indexed by GID (Global ID). Each AIR instruction can have an associated GID stored in `results[index].refinement`.

## Refinement Variants

```
Refinement = union(enum) {
    .scalar      - simple value
    .pointer     - has .to (GID of pointee)
    .optional    - has .to (GID of payload)
    .errorunion  - has .to (GID of payload)
    .region      - has .to (GID of element type), represents array/slice data
    .struct      - has fields: []const Gid
    .union       - has fields: []?Gid
    .allocator   - special for allocator identity tracking
    .fnptr       - function pointer
    .void, .noreturn, .unimplemented
}
```

## Character vs Type

The refinement stores the **character** of the data, not its type. Usually these match, but they can diverge. For example:
- A pointer cast to an integer should retain `.pointer` character (so we can still track what it points to)
- A struct reinterpreted as a region should retain `.struct` character

This allows analysis to track data flow correctly even across type casts.

## Spatial vs Referential Structure

Each refinement represents an **abstract piece of memory**. The refinement describes:
1. The CHARACTER of data (pointer, struct, region, etc.)
2. The ANALYTE state (analysis-specific tracking like defined/undefined)

### The `.to` Field

The `.to` field is a **reference** to another GID. It does NOT mean the referenced entity is "inside" or "part of" the current entity spatially.

- `pointer.to` = GID of the pointee (separate memory that the pointer references)
- `region.to` = GID of the element type (the uniform element pattern, not a specific element)
- `optional.to` = GID of the payload value
- `errorunion.to` = GID of the payload value

### Struct Fields

For `.struct`, the `fields: []const Gid` array contains GIDs of each field. These ARE spatially part of the struct - the fields live within the struct's memory.

Example for `Container { items: []u8 }`:
```
Container (.struct)
  fields[0] = items_gid
      items (.pointer)   <-- This IS part of Container's memory
          .to = region_gid
              region (.region)   <-- This is SEPARATE memory the pointer references
                  .to = element_gid
```

## Store Semantics

When storing a value through a pointer:
1. The pointer's `.to` gives the destination GID
2. For pointer-to-pointer stores, update `dest.pointer.to = src.pointer.to` (share the target)
3. Analyte state on the destination is handled by analysis modules via `splat`
4. Analysis modules should NOT traverse `.to` when setting state - each refinement's analyte describes that refinement's own properties

## Architecture: Tag Handlers vs Analysis Modules

Files in `lib/*.zig` (tag.zig, Refinements.zig, Inst.zig, etc.) must NEVER directly manipulate `.analyte.*` fields. All analyte manipulation is the exclusive responsibility of analysis modules in `lib/analysis/`.

Tag handlers should only:
1. Set up refinement STRUCTURE (pointer, scalar, struct, etc.) via `appendEntity`
2. Call `splat()` or `splatOrphaned()` to dispatch to analysis modules
3. Use structural operations like updating `.to` fields

If you find yourself writing `.analyte.undefined_safety = ...` or `.analyte.memory_safety = ...` in a file outside `lib/analysis/`, STOP - that logic belongs in an analysis module instead.

## Refinement Initialization (splatInit)

When refinements are created via `typeToRefinement`, their analyte fields start as null. The `splatInit` function dispatches to `init` handlers in each analysis module to initialize these fields.

### InitState Enum

```zig
pub const InitState = enum { defined, @"undefined", runtime };
```

- **`defined`**: Interned/global value, known to be defined. Analysis modules set analytes to their "defined" state (e.g., `memory_safety = .interned`, `undefined_safety = .defined`).

- **`undefined`**: Interned/global value, known to be undefined (e.g., `var x: T = undefined`). Analysis modules set analytes to their "undefined" state (e.g., `memory_safety = .interned`, `undefined_safety = .undefined`).

- **`runtime`**: Stack/heap value where the state depends on the operation. Analysis modules may leave analytes as null or set a sensible default. The tag handler (via `splat(.tag_name, ...)`) may further refine the state based on operation semantics.

### When to Use Each State

| Context | InitState | Why |
|---------|-----------|-----|
| Interned constant | `defined` | Comptime values are always defined |
| Global with value | `defined` | Initialized globals are defined |
| Global `= undefined` | `undefined` | Explicitly undefined |
| `alloc` pointee | `runtime` | Tag handler sets stack + undefined state |
| Call return slot | `runtime` | Tag handler determines based on function |
| `store` destination | N/A | Uses existing refinement, no init needed |

### memory_safety and runtime

For `runtime` state, `memory_safety.init` leaves the analyte as null because only the tag handler knows the memory location (stack vs heap vs allocated). Other analysis modules may handle `runtime` differently.

## Copy vs Alias Operations

Most operations create a new refinement, representing new data on the stack. It is strictly the responsibility of `memory_safety` to mark new refinements as `.stack` (or appropriate provenance). Other analysis modules should not set memory provenance.

Alias operations (which don't create new refinements) are rare. `store` is an example - it updates an existing refinement's value or `.to` field rather than creating new data.

## Branch Cloning

When a branch is taken, the entire refinements table is cloned. All refinements in the clone initially have the same values as the parent. Modifications in the branch affect only the branch's copy.

At branch merge, entities created only in the branch (GID >= base_len) may become "orphaned" if not reachable from pre-branch entities.
