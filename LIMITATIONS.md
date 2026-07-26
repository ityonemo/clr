# CLR Limitations

This document tracks known limits in CLR's analyzer model. It should describe
current behavior, and only mention aspirational architecture in passing.

## Stdlib overrides

These are cases where stdlib code would look erroneous to CLR without
instrumented invariant facts.  In the case of user code that looked
like this you would use a declaration (unimplemented; see below) to 
override the CLR issue.  Stdlib is considered privileged and integral
part of the zig language.  For example, use of the allocator interface 
is considered required for memory safety tracking, and allocations
not using the stdlib (for example, with syscalls) are considered to
be untracked by CLR; idiomatic zig code SHOULD use the stdlib, so 
CLR should be considered a tool to push users to build idiomatic and
safe zig code.

Topics in this section should not necessarily be considered missing features
in CLR but rather informational content relating to how CLR overcomes
analysis challenges for core operations.

### Allocators and fd operations

CLR explicitly overrides stdlib's allocator and file descriptor functions as
part of its expected core safety operations. Mutexes may be added to this list in
the future.

### Stdlib HashMap Invariants

Managed `std.HashMap` values are represented by a privileged refinement with
canonical metadata, key, and value storage GIDs. The supported opaque boundary
currently covers `init`, `put`, `get`, `getPtr`, `contains`, `iterator`,
`Iterator.next`, `valueIterator`, `FieldIterator.next`, `keys`/`values`,
`getIndex`, `deinit`, and `deallocate`; selected unmanaged metadata helpers
(fill/remove mutators, `isUsed`/`isFree`/`isTombstone` predicates, and the
assume-capacity storage mutators) remain narrow overrides. This avoids depending
on HashMap's private struct layout and does not weaken general optional unwrap,
pointer arithmetic, packed metadata, or leak checking.

The current model has one representative key slot and one representative value
slot per map, not per-entry storage. It covers scalar maps and the tested case
where a stored aggregate owns one pointer allocation. A map containing multiple
entries with independently managed pointer values may require multiple-source
provenance or per-entry identities and is not yet claimed as fully modeled.
Unlisted HashMap methods are unsupported until given focused boundary tests.

This is a stdlib-specific override. User datastructures could eventually allow 
similar analysis of user datas through a declarative refinement/tag mechanism 
rather than hidden rules.

### Stdlib Byte Reinterpretation

`std.mem.asBytes` intentionally reinterprets a pointer to an object as a byte
view. Generic `bitcast` should continue to preserve the existing pointer shape,
but a call to the stdlib function is explicit evidence that byte indexing is
intended. CLR models `std.mem.asBytes` with a narrow stdlib override that creates
a raw-byte pointer view over the same underlying memory, without changing the
underlying object's multiplicity.

### Process Arguments

`std.process.args` and its `ArgIterator` helpers are treated as an opaque stdlib
boundary. CLR does not model the iterator's internal stdlib layout. Instead, the
args/init path produces an opaque scalar marked `.interned` (this is strictly 
not true, but it is a reasonable elision), and iterator operations such as `skip` 
and `next` only require the compiler-lowered self argument to exist.

## Current Active Gaps (planned to be addressed)

### Declarations

In the future, we may implement a "declarative statement" that
sets the CLR-tracked state of a variable. These declarations should let user
code express facts such as preconditions, postconditions, and local refinement
assertions that CLR cannot infer from ordinary AIR alone.  Currently the 
mechanism for these statements is decided, but the scope or shape of these 
declarations has not been decided.

### Custodial Relationships

CLR does not yet fully model *custody* — a container taking ownership of
independently allocated values on behalf of the code that inserts them. The
representative case is the stdlib HashMap: its refinement carries a single
representative key slot and value slot, not per-entry storage. This covers
scalar maps and the tested case where a stored aggregate owns one pointer
allocation, but a map holding multiple entries with independently managed
pointer values has no per-entry provenance or ownership identity, so their
allocation lifecycles are not individually tracked.

The same limitation applies in principle to any container that takes custody of
distinct owned allocations. A full model needs per-entry (or multiple-source)
provenance so that inserting, retrieving, and destroying individual owned values
can be checked against the correct allocation root. This overlaps with the
multiple-source pointer gap below.

### Multiple-Source Pointers

CLR does not yet model pointers whose target can legally come from multiple
possible source GIDs as a first-class refinement. These pointers can arise after
branch merges, indirect selection, or aggregate propagation where the pointer
value is valid but the precise pointee identity is a set rather than a single
GID.

The intended model is not to reject all operations through such pointers. Some
operations can safely fan out to every possible source, such as scalar writes
that update equivalent state on each target, or narrow recursive structure
updates where every candidate target supports the same field path. Other
operations must remain illegal or require a stronger proof. In particular,
destructive ownership/provenance operations such as `destroy`, `free`, or other
delete-like actions should not be allowed through a multiple-source pointer
unless the operation can prove exactly which allocation root is being consumed.

Until this is represented explicitly, CLR can be either conservative or
imprecise around merged pointer values: it may reject legal scalar/recursive
updates, or it may require narrower special-case handling to avoid applying an
unsafe destructive operation to an ambiguous source.

### Aliasing safety

A strategy for aliasing safety is under consideration and the model for it
is currently being designed.

### Allocator-wide deinit invalidation

When an allocator's backing datastructure (we'll call it "allocator" in this
section) has its `deinit` called and frees all of its outstanding allocations at
once, every allocation made through that allocator should become invalid, and
later use of any of them should be reported as use-after-deinit. CLR currently
models this only for `ArenaAllocator`: deinitializing the arena marks its
allocations invalidated and detects use-after-deinit and double-deinit against
them.

The same lifecycle applies to other allocators whose `deinit` invalidates
outstanding memory (for example `GeneralPurposeAllocator`, or a user allocator
with the same contract). These are not yet given the arena treatment, so
use-after-deinit through a non-arena allocator is not currently caught. The gap
is generalizing the arena-specific deinit-invalidation model to any allocator
that owns and releases its allocations on `deinit`.

Nested allocators — an allocator whose memory comes from another allocator —
fall out of the same generalization and still need targeted coverage.

### `intcast`

`intcast` is not implemented yet.

### Pointer transforms on interned memory

`ptr_add`, `ptr_sub`, `array_to_slice`, and element-pointer construction do not
currently work on `.interned` memory.

### `.recursive` type placeholder

The `.recursive` type placeholder is not fully implemented. It bounds generated
type descriptions, but codegen emits it both for genuine recursive back-edges and
for already-expanded sibling aggregates, and operations that need the full field
shape of a `.recursive`-collapsed type can fail analysis.

### Indirect Function Calls

Function pointer refinements can track possible target functions, and indirect
calls can dispatch through known targets with merged results. Limitations remain:

- vtable-style virtual dispatch is not resolved;
- runtime-computed targets may be unknown;
- function pointers loaded from complex data structures may lose precision;
- merging multiple possible targets can produce false positives when only one
  target is feasible at runtime.

### Call-stack metadata (`ret_addr`, `stack_trace_frames`)

These produce call-stack metadata rather than program data and are currently
unimplemented. That is harmless as long as nothing consumes the result,
but stack-trace-reporting code that reads `@returnAddress()` or walks a
`StackTrace`'s frames will crash the analyzer (these are generally rare,
most programs rely on zig's default stacktrace handling).

### Flagging unsafe code

CLR cannot prove every construct safe, and some operations are inherently
unsafe or unmodeled (for example `ptr_add` and other pointer arithmetic). Rather
than silently pass or hard-error on these, CLR should recognize such patterns and
emit a list of source locations that warrant manual review — a "please check
these" report rather than a proof obligation. This surfacing mechanism is not yet
implemented.

## Memory Safety And Provenance Model (not Rust's)

CLR does track memory safety, but it does not use Rust's ownership model to do
it. The distinction matters for understanding what CLR proves and what it does
not.

Rust enforces safety by ownership: every allocation has exactly one owner at a
time, moving a value transfers that ownership, and the borrow checker forbids
aliasing that could outlive or conflict with the owner. Safety follows from the
rule that there is only ever one path responsible for freeing a value.

CLR instead tracks *provenance*: it follows where a value came from and what
allocation it ultimately refers to, and attaches safety state information to that 
underlying allocation. Multiple values or paths may legitimately refer to the same
allocation. Safety follows from the state of the allocation itself: once any path
frees it, the allocation is considered freed everywhere, and subsequent use or a
second free through any alias is reported.

The practical consequences of choosing provenance over ownership:

- CLR does not reject aliasing or shared references the way a borrow checker
  does; several bindings pointing at one allocation is normal and analyzable.
- CLR does not model move semantics. Nothing is "consumed" by being passed or
  assigned; the allocation simply retains its safety state.
- Because there is no single owner, CLR reasons about the allocation's lifecycle
  (allocated → used → freed) rather than about who is permitted to free it.

In the CLR model, aliasing *safety*, proving that concurrent or overlapping 
aliases do not violate each other's assumptions, is a separate concern from 
allocation lifecycle safety and is not modeled yet, though it is planned.

### Interprocedural Allocation Safety

CLR analyzes across function calls by triggering an analysis pass of the callee
as part of analyzing the caller: arguments are passed in with their current
provenance, the callee's body is analyzed against the same shared state, and both
the returned value's provenance and any mutations made through pointer arguments
flow back to the caller. This lets CLR follow an allocation created in one
function and freed in another, or returned to its caller, without special
annotations.  Notably, it does not build the machinery a classical whole-program 
analyzer would use, such as per-function summaries, a computed call-graph ordering, 
or context-sensitive value tracking across many call sites.

### Global Allocation Leak Detection

Allocations reachable from globals are exempt from normal leak detection outside
the entrypoint. At the entrypoint, CLR also checks allocations stored through
global entities before program exit.

This avoids false positives for intentionally program-lifetime allocations inside
library-style functions, but it also means memory allocated into a global from a
non-entrypoint function and never freed may not be reported there as a leak.

## Deliberate Non-Goals Or Coarse Models

### Custom Allocator Protocols

CLR recognizes standard `std.mem.Allocator`-style allocation operations. Custom
allocator protocols that do not follow those patterns are not tracked. For
example, raw calls to C's `malloc`/`free` are not tracked, and memory obtained
directly from the operating system via syscalls (for example `mmap`ing a page, or
otherwise asking the OS for memory) is not tracked either. Idiomatic Zig routes
allocation through the `std.mem.Allocator` interface, and CLR intentionally only
follows memory that flows through it.

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

### Manual Null-Check Narrowing

CLR narrows optional null-safety only through the structural forms Zig generates —
`if (opt) |x|` capture and the `.?` unwrap (`is_non_null` guard plus
`optional_payload`). It deliberately does not recognize hand-written guards that
prove non-nullness some other way, such as `if (opt != null) { use opt.? } else
@panic()`. These patterns can produce a conservative unchecked-unwrap result. Use
the idiomatic capture/`.?` forms, or an eventual declaration, to express the
invariant instead.

### Derived Pointers And Pointer Arithmetic

Derived pointers — those produced by `ptr_add`/`ptr_sub`, slice extraction, or
field access — are deliberately treated as unsafe to free. They carry a
`root_gid` back to their parent allocation, and free/destroy through a derived
pointer is rejected by design; CLR does not attempt to prove that arithmetic has
returned a pointer to its allocation base. This is a choice, not a missing
feature: an explicit retag mechanism (or a narrow internal stdlib override) could
re-establish base-allocation provenance in the future, but absent that, derived
frees stay illegal.

Pointer arithmetic must operate on a pointer-to-region; non-pointer and
single-item pointer inputs fail loudly with a clear analysis error. CLR does not
perform pointer arithmetic bounds checking and does not prove that a derived
pointer remains within its region.

### Alignment-Cast Lowering

`@ptrCast(@alignCast(...))` lowers into AIR that bitcasts a pointer to an
address-like scalar, masks low bits, and branches on the alignment check. Current
analyses can misinterpret that sequence as ordinary pointer/int dataflow.

CLR deliberately does not model this. The compiler already prevents almost every
way an alignment-cast error could actually occur, and tracking the guard properly
would require carrying pointer provenance through integer values — a
types-through-pointers model, potentially with modular tagging of integers — that
CLR intentionally does not implement (see the "Inst Type Tracking" non-goal in
`CLAUDE.md`). The residual risk is narrow enough to leave unmodeled rather than
build that machinery.

### Integer Overflow

Potential integer overflow in arithmetic operations is not detected.

### Threading And Concurrency

Race conditions, data races, and other concurrency hazards are not tracked. CLR
currently assumes single-threaded execution for analysis purposes.

### Const-Correctness

General const-correctness is handled by Zig. CLR may still (unlikely) need to be 
careful that `@constCast` does not erase safety-relevant dataflow, but it does not 
have a const checker.

### Error Value Propagation

CLR does not try to detect whether errors are handled idiomatically. Zig's error
handling system already enforces explicit propagation or handling at compile
time.

### Exhaustive Switch Coverage

Zig requires exhaustive switches over enums and tagged unions at compile time.
CLR tracks state across switch branches; it does not need to validate missing
cases.
