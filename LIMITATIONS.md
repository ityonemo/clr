# CLR Limitations

## Currently Not Implemented, But Planned

### Recursive Type Tracking

Self-referential types like linked lists (`struct { next: ?*@This(), data: T }`) currently have their recursive fields replaced with `.{ .unknown = {} }` during type extraction. This prevents infinite recursion but loses type information for the recursive field.

**Impact**: Analysis of linked list nodes, tree nodes, or other recursive data structures may have incomplete type information for the recursive pointer fields.

**Future improvement**: Track type names and emit proper forward references instead of unknown placeholders. This would allow full type information to be preserved for recursive types.

**Test plan**: Will be tested alongside ownership tracking for linked lists.

### Complex Type Extraction

Some codegen paths emit `.{ .ty = .{ .unknown = {} } }` when type extraction fails, but the runtime `Type` union doesn't support the `unknown` variant. This causes compile errors in the generated analyzer.

**Affected cases**:
- Some allocator type extraction paths (e.g., when callee reference cannot be resolved)
- Deeply nested struct types that hit recursion limits

**Note**: Most unhandled types correctly use `.{ .ty = .{ .unimplemented = {} } }` which IS supported and will crash at runtime with a clear error if accessed.

**Planned fix**: Replace remaining `.unknown` emissions with `.unimplemented` or add `unknown` variant to `lib/tag.zig` Type union.

### DbgVarVal Analysis Handlers

The `dbg_var_ptr` instruction requires analysis module handlers to retroactively set variable names on analysis states (because in AIR, `dbg_var_ptr` comes after `store` instructions that create the states).

The similar `dbg_var_val` instruction (for non-pointer values) does not currently have analysis handlers. If value variables need names in error messages, handlers would need to be added.

**Investigation needed**: Test whether undefined tracking for value variables (not pointers) produces error messages that would benefit from variable names.

### Cross-Function Global Mutation

Global variables are tracked within individual functions, but mutations to globals by called functions are not propagated back to callers.

**What works**: Reading/writing globals within a single function, detecting use of undefined globals, tracking variant state of global unions within a function.

**What doesn't work**: If function A calls function B which modifies a global, function A's view of the global is not updated after the call returns.

**Planned fix**: May require inter-function dependency analysis to re-analyze callers when callees modify globals.

### Global Pointer Variables (Dereferencing)

When a global variable is itself a pointer (e.g., `var global_ptr: *u8 = &global_value;`), passing the pointer value directly does not properly track that it points to global memory.

**Example that should fail but doesn't**:
```zig
var global_value: u8 = 42;
var global_ptr: *u8 = &global_value;  // Global pointer to global memory

fn do_free(ptr: *u8) void {
    allocator.destroy(ptr);  // Should fail - ptr points to global memory
}

pub fn main() void {
    do_free(global_ptr);  // Passes the VALUE of global_ptr, not &global_ptr
}
```

**Why it's not detected**: The current implementation marks pointees as `.global` when an `int_const` pointer is passed (like `&global_value`). But when passing `global_ptr`, the value is loaded from a global variable first, then passed. The loaded pointer value isn't recognized as pointing to global memory.

**What works**: Passing `&global_value` directly (interned pointer) IS properly tracked.

**Future fix**: Track that global pointer variables contain values that point to global memory. May require analyzing the global's initial value from the InternPool.

### Arena-Style Allocators

Arena-style memory management (allocating from a region, freeing the entire arena at once) is not tracked beyond basic allocator mismatch detection.

**Current behavior**: ArenaAllocator is recognized as a distinct allocator type for mismatch detection, but the "free everything at once" pattern is not specially handled.

**Planned**: Track arena lifetimes so that allocations from an arena don't report as leaks when the arena itself is freed/deinitialized.

### Global Union Initial Values

Global unions don't have their initial variant tracked. The analyzer extracts global types from the InternPool but not their initial values.

**Example that triggers false positive**:
```zig
var global: Value = .{ .int = 42 };  // Initialized with .int

fn set_float() void {
    global = .{ .float = 3.14 };
}

fn checked_access() i32 {
    if (global == .int) {
        return global.int;  // FALSE POSITIVE: reports inactive variant access
    }
    return 0;
}
```

**Why**: After `set_float()` runs, the analysis knows `.float` is active but has no record of `.int` ever being initialized (the initial value isn't tracked). When the tag check proves `.int` is active, we can't update variant_safety because the field entity doesn't exist.

**Workaround**: Ensure the variant you're checking was set in code the analyzer can see (not just the global initializer).

**Future fix**: Extract initial values from the InternPool during codegen and pass them to `initWithGlobals`. The initial value IS available in the InternPool - we just need to read it. This would also allow `const` unions to have their active variant set correctly from initialization.

**Note**: Also need to verify that `var u: MyUnion = undefined;` does the right thing - all variants should be inactive and accessing any field should be an error until a variant is explicitly set.

### Global Field Pointer Tracking (fieldParentPtr)

Pointers to global struct/union fields are computed at compile time and interned. No `struct_field_ptr` instruction is generated, so `fieldparentptr_safety` tracking is not set up.

**Example that won't be tracked**:
```zig
var global_point: Point = .{ .x = 10, .y = 20 };

fn get_x_ptr() *i32 {
    return &global_point.x;  // Interned at comptime, no struct_field_ptr
}

fn get_parent(x_ptr: *i32) *Point {
    return @fieldParentPtr("x", x_ptr);  // Can't verify - no tracking
}
```

**Why**: Zig computes `&global.field` addresses at compile time and interns them directly. The address is embedded in the binary, so no runtime `struct_field_ptr` instruction is generated that would set up our tracking.

**Future fix**: Extract field pointer info from the InternPool during codegen. When an interned pointer points to a global struct field, we could set up `fieldparentptr_safety` based on the InternPool's knowledge of the pointer's origin.

### Global Allocation Leak Detection

Allocations stored in global variables are exempt from leak detection. The analyzer tracks which refinements are reachable from globals and skips leak detection for those allocations entirely.

**Impact**: Memory allocated and stored in a global variable will not be flagged as a leak, even if it is never freed.

**Example**:
```zig
var global_ptr: ?*u8 = null;

pub fn main() void {
    global_ptr = allocator.create(u8);  // Not flagged as leak
    // Never freed - but no error reported
}
```

**Rationale**: Global-stored allocations often have program lifetime and are intentionally not freed. Detecting "true" leaks at program end would require whole-program analysis.

**Future improvement**: Could add optional program-end leak detection that checks if global-reachable allocations are freed before `main` returns.

### Unimplemented AIR Tags

The following AIR instruction tags are not yet implemented and produce `.unimplemented` refinements:

**Array/Slice operations**:
- `aggregate_init` - Initialize aggregate (array/struct) from elements
- `slice` - Create slice from pointer and length (subslices via `slice_ptr` ARE supported)

**Type conversions**:
- `intcast` - Integer type casting

**Error unions**:
- `wrap_errunion_err` - Wrap error into error union (error propagation)

**Debug/minor** (these produce void and don't affect analysis):
- `dbg_inline_block` - Inlined function debug info
- `memset_safe` - Memory set
- `ret_addr` - Return address for stack traces
- `stack_trace_frames` - Stack trace frame info
- `noop_pruned_debug` - Pruned debug instruction

## Needs Investigation

### Review All Parameters That Should Be Src Type

After refactoring `Store.ptr` from `?usize` to `Src` type (to support globals), other tag struct parameters may also need to be converted to `Src` for consistency and global support:

**Candidates to review**:
- `alloc_destroy.ptr` - currently `usize`, may need `Src` for freeing global allocations
- `dbg_var_ptr.ptr` - currently `?usize`, may need `Src` for global variable names
- `set_union_tag.ptr` - currently `usize`, may need `Src` for global unions

**Already using Src**:
- `Store.ptr` - converted to support globals
- `Load.ptr` - already uses `Src` for globals

**Note**: Not all `ptr` fields need to be `Src`. Some are indices into local results (like `struct_field_ptr.base`) which will never be globals.

### Move Semantics (Stack to Allocated)

What happens when we move a value from stack to allocated memory? For example:
```zig
var stack_val: u8 = 42;  // .stack memory_safety
container.field = stack_val;  // storing into .allocated field
```

The store should preserve the destination's `.allocated` memory_safety, but this needs verification and testing.

### Free Checking at Program End vs Function End

Currently, leak detection happens at function close (`onFinish`). Should we instead only perform free-checking at the end of the whole program? This would allow for patterns where allocations are returned to callers and freed elsewhere.

**Questions**:
1. How do we track allocations that are intentionally returned to the caller?
2. Should `returned` allocations be exempt from local leak detection?
3. Is there a use case for "program-level" leak detection vs "function-level"?

### Tombstone for Out-of-Scope Functions

When a function returns, do we need a `.tombstone` state for refinements that are no longer valid? This could prevent issues where:
1. A pointer to a local variable escapes (stack escape)
2. After the function returns, the pointer's target is no longer valid
3. Accessing through that pointer should be detected as use-after-free

**Questions**:
1. Should we mark all stack allocations as "tombstoned" when a function returns?
2. How does this interact with the existing stack escape detection?
3. Would this overlap with or replace the current stack pointer tracking?

### Review Function Branching Architecture

The current early_returns implementation stores full State objects (with cloned refinements) at each return point, then merges them at function end. This allows detecting "conflicting" states when different return paths have different defined/undefined states for in-out parameters.

**Questions to review**:
1. Is storing full State objects the right approach, or should we only store what's needed (refinements)?
2. The `splatMergeEarlyReturns` function rebuilds clean States because stored States may have stale pointers (results, branch_returns point to freed memory). Is there a cleaner architecture?
3. Should `branchIsUnreachable` be split into variants for cond_br/switch_br vs early_returns contexts?
4. The current approach iterates over all results and recursively merges. For large functions, this could be expensive. Is there a more targeted approach?

## Desired But Unknown How to Implement

### Ownership Transfer to Objects or Collections

Passing allocation responsibility to objects or collections (e.g., inserting into an ArrayList that takes ownership) is not tracked. The analyzer cannot determine when an object or collection becomes responsible for freeing memory.

## Not Planned for Near Future

### Alignment Protection

Detection of alignment violations (e.g., casting a pointer to a type with stricter alignment requirements) is not tracked.

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

Slice and array bounds checking is not performed. Out-of-bounds access on regions is not detected.

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

General pointer arithmetic safety is partially implemented. `ptr_add` and `ptr_sub` are blocked on pointers that don't point to regions (i.e., single-item pointers from `create`). Pointer arithmetic within allocated regions is allowed.

Full pointer arithmetic bounds checking (ensuring the result stays within the region) is not tracked.

### Resource Cleanup Beyond Memory

Non-memory resources (file handles, sockets, mutexes) that aren't properly closed/released are not tracked.

### Integer Overflow

Potential integer overflow in arithmetic operations is not detected.

### ConstCast

@constCast may break assumptions that CLR is able to make about code/data propagation through dataflow.

### Per-Element Region Tracking

Arrays and slices use a "uniform region" model where all elements share the same refinement state. This is a deliberate design choice, not a limitation to be improved:

- Setting ANY element marks ALL elements as defined
- If ANY element is undefined, accessing ANY element reports undefined
- Calling a setter function on one element assumes all elements are set

Example:
```zig
var arr: [3]u8 = undefined;
arr[0] = 42;  // System assumes ALL elements are now defined
return arr[2];  // No error (conservative: assumes defined)
```

This trade-off allows analysis to scale to any array size. Per-element tracking would require tracking N refinements per array, which doesn't scale and adds complexity without significant benefit.

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
