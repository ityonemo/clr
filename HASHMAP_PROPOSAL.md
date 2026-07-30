# HashMap Custodial Relationships

## Purpose

This document records a proposed extension to CLR's privileged `std.HashMap`
model. It is design-only: no implementation is implied by this document.

CLR remains a provenance and allocation-lifecycle analyzer. A HashMap
relationship is not Rust-style ownership transfer and does not make `put`
consume its value. Containment alone never decides whether the map, the caller,
or some other component is responsible for a pointed-to allocation's eventual
deletion.

## Membership Is Not Custody

Each successful `put` records an individual key/value entry with its actual
provenance. The existing model's one canonical key and value slot cannot
represent cleanup correctly: every iterator result can otherwise appear to
refer to the same allocation.

Insertion alone makes no custody claim. Both patterns are valid:

```zig
const item = try allocator.create(Item);
try map.put(1, item);
allocator.destroy(item); // The map was only an index.
```

```zig
const item = try allocator.create(Item);
try map.put(1, item);

var it = map.valueIterator();
while (it.next()) |entry| {
    allocator.destroy(entry.*); // Cleanup through the map.
}
```

## Uniformity And Custody Eligibility

HashMap is a consumer of a general heterogeneous-pointer model, not the place
where that model is invented. A map may contain values whose pointer provenance
comes from different roots and whose pointed-to structures differ. Its entry,
iterator, and retrieval views must therefore preserve a composition of the
possible source GIDs rather than collapse them into one representative slot.

The composition determines whether a HashMap can be responsible for its
pointed-to values:

- A heterogeneous pointer has candidates with incompatible cleanup-relevant
  provenance. Its containing HashMap cannot be responsible for deletion.
  HashMap operations remain agnostic about those pointed-to allocations.
- A uniform pointer has compatible candidate provenance and is eligible for a
  custodial relationship. Eligibility is not responsibility: a uniform map may
  still be an index whose caller deletes every value.

For a uniform pointer, responsibility must be established separately by the
program's cleanup behavior or a future explicit declaration. A map-derived
destructive operation is such cleanup behavior; `put`, removal, iteration, or
aliasing alone are not.

## Agnostic Views And Deletion

For a heterogeneous pointer, HashMap's view of deletion is necessarily
agnostic. A value being contained in a map does not imply that `map.deinit()`,
removal, iteration, or any other map operation must delete the value's
pointed-to allocation. Those operations are responsible only for the map's own
backing storage unless their actual code performs a separate `free` or
`destroy`.

For example, the following path is provenance information, not an implicit
destructor policy:

```text
map -> valueIterator() -> next() -> entry -> entry.*
```

If user code subsequently calls `allocator.destroy(entry.*)`, generic
multiple-source-pointer analysis determines whether the operation is uniform.
It does not model one unknown concrete member selection. It conceptually
iterates every source root, validates the allocator action for each root, and
marks every root freed globally. A later destroy through any alias is an
ordinary double-free. HashMap does not provide a special ownership exemption
for either operation.

This is deliberately compatible with CLR's existing model: receiving a pointer,
inserting it into a container, or obtaining an alias does not itself create a
responsibility to free it.

## Heterogeneous Pointer Composition

Generic heterogeneous pointers must retain their possible source GIDs as an
explicit composition. Operations can then be classified by whether they are
sound for every candidate source:

- scalar mutation may fan out when each target supports it;
- recursive operations may proceed when the same field path is valid for every
  target;
- destructive operations may fan out only when every source has the same
  `allocator_gid` and supports the same allocation method, such as
  `create`/`destroy` or `alloc`/`free`.

The destructive fan-out is a conceptual loop in the analyzer, not a statement
that one concrete runtime pointer denotes every source at once:

```text
for composed_pointer.sources |root|:
    validate allocator action against root
    mark root freed
```

After the conceptual loop, CLR deliberately collapses the state around every
candidate root as freed. This is the same conservative policy used when an
ambiguous free state propagates through aliases or merged control flow.

An allocation root may appear in more than one entry as an alias. CLR should
preserve those entries separately. If cleanup reaches both entries, the second
cleanup is a genuine double-free and must be reported.

## Intended Initial Boundary

The first implementation should introduce generic heterogeneous-pointer
composition, then model managed HashMap insertion and views using that generic
representation. It must distinguish heterogeneous, deletion-agnostic entries
from uniform entries that are merely eligible for custody. It must not infer
deletion responsibility from `put`, map deinitialization, removal, iteration,
or mere aliasing.

Once a uniform map has a map-derived destructive operation, it is custodial.
Before `deinit`, CLR performs a conceptual sweep over its composed member
sources and applies the compatible deletion action to every remaining live
root. This requires no recognition of a syntactic user cleanup loop; it is the
container-level consequence of the established custodial relationship.

Cleanup performed through vtables and objects whose cleanup protocol depends on
an allocator stored inside the value remain future work. Once implemented,
those boundaries belong in `LIMITATIONS.md`.
