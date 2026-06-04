# CLR Limitations

This document tracks known limits in CLR's analyzer model. It should describe
current behavior, not aspirational architecture. Use `AGENTS.md` for the active
sprint baseline and `FIXES_LOG.md` for completed fixes.

## Stdlib overrides

These are cases where stdlib code would look erroneous to CLR without
stdlib-specific invariant facts. Usually, they are guarded by invariant
conditions that can't be tracked simply by CLR, for example a size check
guaranteeing nonnullability of an optional. In the case of user code that looked
like this you would use a declaration (see below) to override the CLR issue.

Topics in this section should not necessarily be considered missing features
in CLR but rather informational content relating to the limitations of CLR.

### Allocators and fd operations

CLR explicitly overrides stdlib's allocator and file descriptor functions as
part of its expected normal operation. Mutexes may be added to this list in
the future.

### Stdlib HashMap Invariants

`HashMapUnmanaged.header`, `getIndex`, and some capacity-assumed insertion
helpers depend on HashMap invariants that ordinary AIR does not expose directly.
CLR models those paths through narrow stdlib overrides instead of weakening
general optional unwrap, pointer arithmetic, packed metadata, or leak checking.

This is a stdlib-specific override. User code should eventually express similar
facts through a declarative refinement/tag mechanism rather than hidden
HashMap-specific rules.

### Stdlib Byte Reinterpretation

`std.mem.asBytes` intentionally reinterprets a pointer to an object as a byte
view. Generic `bitcast` should continue to preserve the existing pointer shape,
but a call to the stdlib function is explicit evidence that byte indexing is
intended. CLR models `std.mem.asBytes` with a narrow stdlib override that creates
a raw-byte pointer view over the same underlying memory, without changing the
underlying object's multiplicity.

## Current Active Gaps (planned to be addressed)

### Declarations

In the future, we will be implementing a "declarative statement" that
sets the CLR-tracked state of a variable. These declarations should let user
code express facts such as preconditions, postconditions, and local refinement
assertions that CLR cannot infer from ordinary AIR alone.

### File Descriptor Alias And Closure Propagation

FD state is tracked for the covered stdlib/POSIX open, close, read/write,
flock, dup, socket, accept, epoll, and pipe patterns. Basic propagation through
stores, aggregate initialization, returned values, and scalar fields is covered
by the current integration suite.

The remaining architecture work is descriptor aliasing and closure propagation.
The broader model is still not a general descriptor alias analysis: copies or
transformations that lose the tracked scalar FD refinement can also lose
close/use/leak state, and raw syscalls or custom wrappers are only checked when
they lower through a recognized override.

### Stack Escape And Returned Aggregate Provenance

Returning pointers inside structs, unions, or globals is still conservative in
some cases. Basic fieldParentPtr recovery for structs, unions, and global
struct/union fields is covered, but returned pointers through aggregate values
and merged pointers can still lose enough provenance to produce false positives.

This area overlaps with stack lifetime tracking: CLR detects some escaping stack
pointers, but it does not yet have a complete tombstone model for stack memory
after function return.

### Alignment-Cast Lowering

`@ptrCast(@alignCast(...))` lowers into AIR that bitcasts a pointer to an
address-like scalar, masks low bits, and branches on the alignment check. Current
analyses can misinterpret that sequence as ordinary pointer/int dataflow.

Planned mitigation: add an AIR-to-analyzer interceptor that recognizes this
compiler-generated alignment guard and no-ops the failure branch for current
analyses. A future `alignment_safety` module can track the actual alignment
proof separately.

### Pointer Arithmetic And Retagging

`ptr_add` and `ptr_sub` both produce derived pointers into the same region.
Neither operation proves that the pointer has returned to the allocation base.
Freeing memory through a derived pointer should remain invalid unless a future
explicit retag feature, or a narrow internal stdlib override, reestablishes
base-allocation provenance.

Pointer arithmetic must operate on a pointer-to-region. Non-pointer inputs and
single-item pointer inputs should fail loudly with a clear analysis error.

CLR does not perform pointer arithmetic bounds checking. It does not prove that
the derived pointer remains within the region.

## Memory And Provenance Model Limits

### No Rust-Style Ownership Transfer

CLR tracks provenance and safety, not exclusive ownership. More than one value
or path may refer to the same allocation root GID. Once any path marks that root
freed, the allocation's state collapses around that GID and later checks should
use the collapsed state.

Consequences:

- allocation root GID is the allocation identity;
- `allocator_gid` is only for allocator mismatch detection;
- pointer values should point at already-existing GIDs in their destination
  `Refinements` table;
- cross-table operations may merge/import structure, but should not transfer a
  pointer value's `.to` target across tables.

Aliasing safety is separate from allocation memory safety and is not fully
implemented yet.

### Interprocedural Allocation Safety

CLR has limited call-boundary propagation and several common allocation-return
and callee-free cases are covered. It still does not perform full
interprocedural analysis with function summaries, call graph ordering, and
context-sensitive value tracking.

This means some patterns involving allocations returned through complex
aggregates, stored in globals, or modified by callees can still be conservative
or incomplete.

### Global Allocation Leak Detection

Allocations reachable from globals are exempt from normal leak detection outside
the entrypoint. At the entrypoint, CLR also checks allocations stored through
global entities before program exit.

This avoids false positives for intentionally program-lifetime allocations inside
library-style functions, but it also means memory allocated into a global from a
non-entrypoint function and never freed may not be reported there as a leak.

### Cross-Function Global Mutation

Mutations to globals are tracked within a function, but a caller's view of a
global is not generally updated after a callee mutates it.

Supporting this well likely requires inter-function dependency analysis or
callee summaries for global side effects.

### Global Pointer Values

Direct global addresses such as `&global_value` are handled better than pointer
values loaded from global pointer variables. A loaded global pointer can lose the
fact that its value points to global memory.

Future work should preserve pointer-valued global initializer facts from the
InternPool and import them when the global pointer value is loaded.

### Nested Arena Allocators

Arena-style memory management is mostly tracked, including init, deinit,
allocator retrieval, leak detection, use-after-deinit, double-deinit, and
cross-allocator mismatch detection. Nested arenas, where an arena is backed by
another arena, remain less certain and need targeted tests before being treated
as covered.

## Variant, Optional, And Aggregate Limits

### Global Union Initial Values

Basic global union initial active variants are imported from the generated
global refinement structure and are covered for the current integration suite.

This area is still sensitive to InternPool shape. If a global union initializer
cannot be represented as a `GlobalDef.union_field` with an active field entity,
CLR may lose the initial active variant and later checked field access can be
conservative or incomplete.

Future work should keep adding targeted global-union initializer shapes as they
appear in real AIR.

### Optional Pointer AIR Semantics

AIR instructions such as `is_non_null`, `is_null`, and `optional_payload` can
appear around optional wrappers and pointer-like values. CLR's null-safety model
should continue to distinguish optional state from raw pointer value state.

Open questions:

1. When does Zig emit these instructions for pointer values rather than
   optionals?
2. Which generated pointer checks should be modeled by null safety, and which
   should be ignored or handled by another module?
3. How should alignment or sentinel checks that look optional-like in AIR be
   separated from actual optional unwraps?

## AIR And Codegen Limits

### Partially Implemented AIR Tags

Some AIR instruction tags are handled only enough to avoid crashing and may
produce `.unimplemented` refinements:

- `dbg_inline_block`
- `intcast`
- `ret_addr`
- `stack_trace_frames`

The debug and stack-trace tags are unlikely to affect ordinary safety cases.
`intcast` is more significant: it currently does not inherit undefined or other
value safety state from its operand, so safety state can be lost across integer
casts until that handler is completed.

### Codegen Type Extraction Gaps

Some generated analyzer tags depend on extracting precise type information from
AIR and the InternPool. When type extraction fails, CLR should prefer a loud
`.unimplemented` refinement or analysis failure over guessing a scalar shape.

Known examples include allocator `create` payload type extraction and slice
element type reconstruction. These are correctness boundaries in the AIR-to-CLR
translation layer rather than intentional safety allowances.

### Indirect Function Calls

Function pointer refinements can track possible target functions, and indirect
calls can dispatch through known targets with merged results. Limitations remain:

- vtable-style virtual dispatch is not resolved;
- runtime-computed targets may be unknown;
- function pointers loaded from complex data structures may lose precision;
- merging multiple possible targets can produce false positives when only one
  target is feasible at runtime.

### Custom Allocator Protocols

CLR recognizes standard `std.mem.Allocator`-style allocation operations. Custom
allocator protocols that do not follow those patterns are not generally tracked.
For example, if you do raw calls to C's `malloc`/`free` these memory will not be
tracked.

## Deliberate Non-Goals Or Coarse Models

### Per-Element Region Tracking

Arrays and slices use a uniform region model: all elements share one refinement
state. Setting any element can mark the whole region defined; if the region is
undefined, accessing any element can report undefined.

This is deliberate. Per-element tracking would require one refinement per
element and does not scale to large arrays or slices. This may cause some
problems if Zig's stdlib (or user code) uses an array instead of a tuple.
For example, Zig's stdlib generates stdout/stdin as a pair using an array.

### Bounds Checking

CLR does not perform array, slice, or pointer bounds checking. Zig's runtime
safety checks and the compiler's own semantics cover some of this space, but CLR
does not model it independently.

### Integer Overflow

Potential integer overflow in arithmetic operations is not detected.

### Threading And Concurrency

Race conditions, data races, and other concurrency hazards are not tracked. CLR
currently assumes single-threaded execution for analysis purposes.

### Const-Correctness

General const-correctness is handled by Zig. CLR may still need to be careful
that `@constCast` does not erase safety-relevant dataflow, but it is not a
separate const checker.

### Error Value Propagation

CLR does not try to detect whether errors are handled idiomatically. Zig's error
handling system already enforces explicit propagation or handling at compile
time.

### Exhaustive Switch Coverage

Zig requires exhaustive switches over enums and tagged unions at compile time.
CLR tracks state across switch branches; it does not need to validate missing
cases.
