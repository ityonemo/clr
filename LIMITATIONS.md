# CLR Limitations

## Currently Not Implemented, But Planned

### Recursive Type Tracking

Self-referential types like linked lists (`struct { next: ?*@This(), data: T }`) currently have their recursive fields replaced with `.{ .unknown = {} }` during type extraction. This prevents infinite recursion but loses type information for the recursive field.

**Impact**: Analysis of linked list nodes, tree nodes, or other recursive data structures may have incomplete type information for the recursive pointer fields.

**Future improvement**: Track type names and emit proper forward references instead of unknown placeholders. This would allow full type information to be preserved for recursive types.

**Test plan**: Will be tested alongside ownership tracking for linked lists.

### GeneralPurposeAllocator Type Complexity

Using `std.heap.GeneralPurposeAllocator` generates deeply nested struct types that hit the recursion depth limit. The codegen emits `.{ .unknown = {} }` for these types, but the runtime `Type` union doesn't yet support the `unknown` variant.

**Status**: Codegen succeeds, but the generated analyzer fails to compile due to missing `unknown` in `Type` union.

**Planned fix**: Add `unknown` variant to `lib/tag.zig` Type union to handle complex types that exceed the depth limit.

### Runtime Allocator Type Identification

For runtime allocators (e.g., `var gpa = GeneralPurposeAllocator(.{}){}` or `const alloc = fba.allocator()`), we currently return a generic "Allocator" marker instead of the specific type.

**Impact**: Mismatched allocator detection only works when at least one allocator is a comptime constant (like `std.heap.page_allocator`). Two different runtime allocators will both show as "Allocator" and won't be detected as mismatched.

**Planned improvements**:
1. **Global/const allocator tracking**: If an allocator is stored in a global or const variable, trace through the store to find the vtable source
2. **Allocator type through typing system**: Track allocator types through the slot typing system - when a variable is declared as a specific allocator type, propagate that type label to any `Allocator` derived from it via `.allocator()`
3. **Vtable field tracing**: Search for `struct_field_ptr_index_1` (vtable field) stores to find the vtable global for runtime-constructed Allocator structs

### Struct/Union Field Clobber Memory Leaks

When a struct or union field containing an allocated pointer is overwritten (clobbered) without first freeing the old value, this might create a memory leak. This case is not currently tested.

**Example**:
```zig
var container: Container = .{ .ptr = try allocator.create(u8) };
container.ptr = try allocator.create(u8);  // Leaks the first allocation!
```

**Planned**: Add test cases for detecting leaks when struct/union pointer fields are clobbered.

### Struct Field Tracking

Struct field tracking is implemented for most cases:

**What works**:
- `struct_field_val`: Extracting a field value from a struct by value is tracked
- `struct_field_ptr`: Field pointers inherit undefined state from their parent field and propagate changes back
- Field-level undefined detection when accessing struct fields
- Origin tracking prevents freeing field pointers (only parent allocations can be freed)

**What doesn't work**:
- Memory safety for individual fields beyond origin tracking is not implemented

### DbgVarVal Analysis Handlers

The `dbg_var_ptr` instruction requires analysis module handlers to retroactively set variable names on analysis states (because in AIR, `dbg_var_ptr` comes after `store` instructions that create the states).

The similar `dbg_var_val` instruction (for non-pointer values) does not currently have analysis handlers. If value variables need names in error messages, handlers would need to be added.

**Investigation needed**: Test whether undefined tracking for value variables (not pointers) produces error messages that would benefit from variable names.

### Global Types and Variables

Global variables and types are not fully tracked through the analysis.

**Planned fix**: May require more sophisticated analysis for generating dependency understanding and retriggering analysis on functions when globals change.

### Interned Pointer Arguments (Memory Safety)

When a compile-time constant pointer is passed as a function argument (e.g., a pointer to a global or string literal), the memory safety analysis doesn't set any state on it. This is probably correct for most cases, but edge cases may exist.

**Example**: Passing `&global_var` as an argument - the memory safety analysis currently does nothing. This may need special handling if we want to track globals.

**Investigation needed**: Determine if interned pointer arguments need memory safety state initialization.

### Region Allocation/Freeing

Region-based memory management (allocating from a region, freeing entire regions) is not tracked.

**Planned**: Support for region allocators and tracking region lifetimes.

### Loops

Loop analysis is not implemented. The analyzer doesn't perform fixed-point iteration to reach stable states for loop bodies.

**Impact**: Variables modified in loops, allocations in loops, and loop-dependent control flow are not properly analyzed.

**Planned**: Implement fixed-point iteration for loops with proper state merging.

### Branch Merge with Heap-Allocated Structs

When allocating struct types via `allocator.create(StructType)` combined with error handling (`catch return`), the branch merge logic can crash. The issue occurs in `mergeRefinementRecursive` when one branch has a struct refinement while another has a different refinement type (e.g., scalar).

**Trigger conditions**:
1. Allocate a struct type via `allocator.create(Container)` returning `?*Container`
2. Use error handling (`catch return 1`) which creates a conditional branch
3. During `cond_br` merge, branches have mismatched refinement types

**Error**: `access of union field 'struct' while field 'scalar' is active`

**Workaround**: Use stack-allocated containers with separately heap-allocated fields:
```zig
// Instead of: var container = allocator.create(Container) catch return 1;
var container: Container = undefined;
container.ptr = allocator.create(u8) catch return 1;
```

**Planned fix**: Improve `mergeRefinementRecursive` to handle cases where branch refinements have different types, possibly by finding a common refinement or using a fallback.

### Unimplemented AIR Tags

The following AIR instruction tags are not yet implemented and produce `.unimplemented` refinements:

**Array/Slice operations**:
- `aggregate_init` - Initialize aggregate (array/struct) from elements
- `array_to_slice` - Convert array to slice
- `slice` - Create slice from pointer and length
- `slice_len` - Get length of slice

**Type conversions**:
- `intcast` - Integer type casting
- `ptr_add` - Pointer arithmetic

**Error unions**:
- `wrap_errunion_err` - Wrap error into error union
- `wrap_errunion_payload` - Wrap payload into error union

**Debug/minor**:
- `dbg_inline_block` - Inlined function debug info
- `memset_safe` - Memory set (produces void)
- `ret_addr` - Return address for stack traces
- `stack_trace_frames` - Stack trace frame info
- `noop_pruned_debug` - Pruned debug instruction (produces void)

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

### Optional Pointers in AIR

**Investigation needed**: AIR instructions `is_non_null`, `is_null`, and `optional_payload` can apply to BOTH optional types (`?T`) AND pointer types (`*T`). The current implementation handles both cases, but the semantics need further examination:

- For optionals: These instructions check/unwrap the optional wrapper
- For pointers: The semantics are unclear - possibly checking for null pointers?

**Questions to investigate**:
1. When does Zig generate `is_non_null` for a pointer vs an optional?
2. What does `optional_payload` mean when applied to a pointer?
3. Are there cases where pointer null checks should trigger null_safety analysis?

**Current behavior**: The handlers check if the refinement is `.optional` or `.pointer` and only perform null_safety tracking for optionals. Pointers are handled but don't update null_safety state. This may need revision after investigation.

### Pointer Arithmetic

General pointer arithmetic safety is not tracked. (May implement a weak version: disallow pointer arithmetic unless within a tracked region.)

### Resource Cleanup Beyond Memory

Non-memory resources (file handles, sockets, mutexes) that aren't properly closed/released are not tracked.

### Integer Overflow

Potential integer overflow in arithmetic operations is not detected.

### ConstCast

@constCast may break assumptions that CLR is able to make about code/data propagation through dataflow.

## Not Necessary (Handled by Zig)

### Const-correctness

General const-correctness is handled by Zig.

### Error Value Propagation

Tracking whether errors are properly handled is not implemented. Zig's error handling system (with `try`, `catch`, and explicit error returns) already enforces this at compile time.

### Exhaustive Switch Coverage

Zig requires switches on enums and tagged unions to be exhaustive at compile time. Missing cases are a compile error, so CLR does not need to detect:

- Missing enum cases in switch statements
- Missing union tag cases
- Unreachable patterns in switch arms

CLR only needs to track state merging across switch branches, not validate that all cases are covered.
