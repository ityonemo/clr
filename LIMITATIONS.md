# CLR Limitations

## Currently Not Implemented, But Planned

### Standard Library Function Analysis

Currently, CLR only analyzes functions from the user's root module. Standard library functions (start.*, os.*, debug.*, posix.*, fs.*, etc.) are skipped because their AIR instructions contain uninitialized/garbage data in some fields.

**Symptoms**: When processing stdlib functions, the `block` instruction's `ty_pl.ty` field contains garbage (0xAA patterns in debug mode), causing segfaults when trying to extract type information.

**Impact**: Analysis cannot trace through stdlib function calls. If a stdlib function modifies memory or affects control flow, those effects are not tracked.

**Investigation needed**:
- Determine why stdlib AIR has uninitialized instruction fields
- Check if this is an issue with how libclr.so receives data from the compiler
- Consider whether stdlib functions need special handling (they may not have the same type annotation requirements)

**Workaround**: Filter functions by checking if their FQN starts with the root module name, skipping all others.

### Large Return Value Tracking (ret_load, ret_ptr)

Functions that return large values (structs, arrays) use `ret_load` or `ret_ptr` instructions instead of `ret_safe`. These instructions handle returns where the value is returned via a pointer to caller-provided storage.

- `ret_load`: Returns by loading from a pointer (for values that don't fit in registers)
- `ret_ptr`: Returns the pointer where the return value should be stored

**Impact**: Analysis of functions returning large structs or arrays may be incomplete. Return value tracking only works for simple scalar returns via `ret_safe`.

**Planned fix**: Implement handlers for `ret_load` and `ret_ptr` that properly track the data flow through the return pointer.

### Const vs Mutable Pointer Parameter Tracking

The `arg_ptr` mechanism propagates analysis information backwards through pointer parameters to update the caller's slot state. Currently, this happens for ALL pointer parameters, but it should NOT happen for `*const` pointers since the callee cannot modify through them.

**Impact**: Information may incorrectly flow backwards through const pointers, leading to potential false negatives or incorrect analysis results.

**Planned fix**: Add `is_const: bool` field to the arg payload and only set `arg_ptr` when `!is_const`.

### Refinement Type Tracking

Refinements track basic type structure (scalar, pointer, optional) but don't capture the full Zig type. This makes it difficult to reason about:
- Whether a pointer is const or mutable
- Struct vs slice vs primitive types
- Specific integer/float types
- How to correctly propagate analysis information through different type categories

**Current state**: Codegen extracts type info from the InternPool for interned values and passes it via the `Type` union. Refinements can be scalars, pointers, optionals, or regions with nested structure.

**Impact**: Type-specific analysis decisions are limited. For example, we can't distinguish `*u8` from `*u32`.

**Planned fix**: Extend the type system to capture more detail when needed for specific analyses.

### Runtime Allocator Type Identification

For runtime allocators (e.g., `var gpa = GeneralPurposeAllocator(.{}){}`), we currently return a generic "Allocator" marker instead of the specific type like "GeneralPurposeAllocator".

**Impact**: Mismatched allocator detection only works when at least one allocator is a comptime constant. Two different runtime allocators will both show as "Allocator" and won't be detected as mismatched.

**Planned improvements**:
1. **Global/const allocator tracking**: If an allocator is stored in a global or const variable, trace through the store to find the vtable source
2. **Allocator type through typing system**: Track allocator types through the slot typing system - when a variable is declared as a specific allocator type, propagate that type label to any `Allocator` derived from it via `.allocator()`
3. **Vtable field tracing**: Search for `struct_field_ptr_index_1` (vtable field) stores to find the vtable global for runtime-constructed Allocator structs

### Struct Field Tracking

Struct field tracking is partially implemented:

**What works**:
- `struct_field_val`: Extracting a field value from a struct by value is tracked
- Field-level undefined detection when accessing struct fields

**What doesn't work**:
- `struct_field_ptr`: The undefined analysis doesn't have a handler for struct_field_ptr, so stores through field pointers may not be properly tracked
- Memory safety for individual fields is not tracked

**Example not detected**: Setting `.x` but not `.y` through field pointers (as opposed to struct literals) may not be caught.

**Planned fix**: Add undefined analysis handler for `struct_field_ptr` to track stores through field pointers.

### Global Types and Variables

Global variables and types are not fully tracked through the analysis.

**Planned fix**: May require more sophisticated analysis for generating dependency understanding and retriggering analysis on functions when globals change.

### Region Allocation/Freeing

Region-based memory management (allocating from a region, freeing entire regions) is not tracked.

**Planned**: Support for region allocators and tracking region lifetimes.

### Branching and Control Flow

Basic infrastructure for conditional branching (`cond_br`) exists, with partial support in undefined analysis:

**What works**:
- Simple if/else branches that both assign or both leave undefined
- Detection of inconsistent branches (one assigns, one doesn't)
- Merging undefined states at join points for scalar and optional types

**What doesn't work**:
- Memory safety analysis doesn't merge states at join points
- Multiple branches targeting the same block (complex control flow)
- Nested conditionals may have edge cases
- Early returns from one branch need more testing

### Loops

Loop analysis is not implemented. The analyzer doesn't perform fixed-point iteration to reach stable states for loop bodies.

**Impact**: Variables modified in loops, allocations in loops, and loop-dependent control flow are not properly analyzed.

**Planned**: Implement fixed-point iteration for loops with proper state merging.

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

Basic optional type tracking exists (refinements can be `.optional` with an inner type), but unwrapping optionals without null checks is not detected. The `is_non_null` and `optional_payload` instructions are currently no-ops.

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
