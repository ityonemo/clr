# CLR Limitations

## Currently Not Implemented, But Planned

### Const vs Mutable Pointer Parameter Tracking

The `arg_ptr` mechanism propagates analysis information backwards through pointer parameters to update the caller's slot state. Currently, this happens for ALL pointer parameters, but it should NOT happen for `*const` pointers since the callee cannot modify through them.

**Impact**: Information may incorrectly flow backwards through const pointers, leading to potential false negatives or incorrect analysis results.

**Planned fix**: Add `is_const: bool` field to the arg payload and only set `arg_ptr` when `!is_const`.

### Slot Type Tracking

Slots don't track what type of value they contain. This makes it difficult to reason about:
- Whether a slot holds a pointer vs a value type
- Whether a pointer is const or mutable
- Struct vs slice vs primitive types
- How to correctly propagate analysis information through different type categories

**Impact**: Type-specific analysis decisions cannot be made, leading to overly conservative or incorrect behavior in some cases.

**Planned fix**: Allow analyzers to express a richer type system at runtime. Instead of embedding static type info during codegen, analyzers can build and refine their own type representations as they process instructions.

### Runtime Allocator Type Identification

For runtime allocators (e.g., `var gpa = GeneralPurposeAllocator(.{}){}`), we currently return a generic "Allocator" marker instead of the specific type like "GeneralPurposeAllocator".

**Impact**: Mismatched allocator detection only works when at least one allocator is a comptime constant. Two different runtime allocators will both show as "Allocator" and won't be detected as mismatched.

**Planned improvements**:
1. **Global/const allocator tracking**: If an allocator is stored in a global or const variable, trace through the store to find the vtable source
2. **Allocator type through typing system**: Track allocator types through the slot typing system - when a variable is declared as a specific allocator type, propagate that type label to any `Allocator` derived from it via `.allocator()`
3. **Vtable field tracing**: Search for `struct_field_ptr_index_1` (vtable field) stores to find the vtable global for runtime-constructed Allocator structs

### Struct Field Tracking

Uninitialized struct fields and memory safety for individual fields are not tracked. For example, setting `.x` but not `.y` on a struct is not detected.

**Planned fix**: Requires the richer runtime type system (see Slot Type Tracking above).

### Global Types and Variables

Global variables and types are not fully tracked through the analysis.

**Planned fix**: May require more sophisticated analysis for generating dependency understanding and retriggering analysis on functions when globals change.

### Region Allocation/Freeing

Region-based memory management (allocating from a region, freeing entire regions) is not tracked.

**Planned**: Support for region allocators and tracking region lifetimes.

### Loops and Control Flow

Loops and branching control flow are not properly analyzed. The analyzer currently follows straight-line code but doesn't perform fixed-point iteration for loops or merge states across different control flow paths.

**Planned**: Implement proper control flow analysis with fixed-point iteration for loops and state merging at join points.

## Desired But Unknown How to Implement

### Ownership Transfer to Objects or Collections

Passing allocation responsibility to objects or collections (e.g., inserting into an ArrayList that takes ownership) is not tracked. The analyzer cannot determine when an object or collection becomes responsible for freeing memory.

## Not Planned for Near Future

### Full Interprocedural Analysis

CLR performs limited interprocedural analysis through the `arg_ptr` mechanism for pointer parameters. Full interprocedural analysis that tracks all values across function boundaries is not planned.

Full interprocedural analysis would involve:
- Building a complete call graph
- Analyzing callees before callers
- Creating "summaries" for each function (what it allocates, frees, escapes, modifies)
- Tracking values through arbitrary call depth
- Context-sensitive analysis: `foo(allocA)` vs `foo(allocB)` tracked separately

**Workaround**: The analyzer focuses on detecting issues within individual function bodies and at function call boundaries.

### Cross-Module Analysis

Analysis is performed on a per-module basis. Tracking allocations or pointers that cross module boundaries (e.g., allocated in library A, freed in library B) is not supported.

### Complex Aliasing Analysis

Pointer aliasing analysis is limited. If two pointers may alias the same memory but through different paths, the analyzer may not track this relationship correctly.

### Threading and Concurrency Analysis

Detection of race conditions, data races, or other concurrency-related memory safety issues is not supported. The analyzer assumes single-threaded execution.

### Indirect Function Calls

Function pointers and virtual method dispatch are not tracked. Allocator operations performed through indirect calls may not be detected.

### Custom Allocator Protocols

The analyzer specifically recognizes `std.mem.Allocator` operations. Custom allocator interfaces that don't use the standard protocol are not tracked.

### Bounds Checking

Slice and array bounds checking is not performed. Out-of-bounds access is not detected.

### Optional/Null Safety

Unwrapping optionals without null checks is not detected.

### Union Variant Safety

Accessing the wrong variant of a tagged union is not detected. (This might become necessary in the future.)

### Pointer Arithmetic

General pointer arithmetic safety is not tracked. (May implement a weak version: disallow pointer arithmetic unless within a tracked region.)

### Resource Cleanup Beyond Memory

Non-memory resources (file handles, sockets, mutexes) that aren't properly closed/released are not tracked.

### Integer Overflow

Potential integer overflow in arithmetic operations is not detected.

## Not Necessary (Handled by Zig)

### Error Value Propagation

Tracking whether errors are properly handled is not implemented. Zig's error handling system (with `try`, `catch`, and explicit error returns) already enforces this at compile time.
