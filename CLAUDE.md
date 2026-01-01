# CLR Project

## Project Structure

- `zig/` - Zig compiler submodule (DO NOT MODIFY - all changes must be non-AI)

## Build Commands

- `zig/b` - Builds the custom Zig compiler (uses `zig build --zig-lib-dir lib`)
- `zig/z` - Runs the custom Zig compiler from `zig-out/bin/zig`
- `zig build` - Builds libclr.so (the AIR plugin)
- `zig build test` - Runs unit tests for libclr
- `./run_integration.sh` - Runs BATS integration tests
- `./run_one.sh <test_file>` - Run a single test case (generates `.air.zig` file in project root)
- `./dump_air.sh <source_file> <function_name> [num_lines]` - Dump AIR for a specific function
- `./clear.sh` - Clean up generated `.air.zig` files and other build artifacts

### Debugging AIR

To view the raw AIR for a function:
```sh
./dump_air.sh test/cases/undefined/basic/assigned_before_use.zig assigned_before_use.main 40
```

This shows the instruction indices, tags, and nesting structure. Block bodies may have indices that are **higher** than post-block instructions (e.g., block at %10 may contain %16-%23, while %11-%15 come after the block).

## Testing

### Unit Tests

There are TWO sets of unit tests in different locations:

**1. Codegen/DLL tests (`src/`)** - Run with:
```sh
zig build test
```
These test the code generation and DLL infrastructure.

**2. Runtime library tests (`lib/`)** - Run with:
```sh
zig test lib/lib.zig
```
These test the runtime analysis logic (tag handlers, memory_safety, undefined tracking).

**IMPORTANT**: `zig build test` only runs `src/` tests. Always run BOTH when modifying analysis logic.

### Integration Tests (BATS)

Integration tests use [BATS](https://github.com/bats-core/bats-core) (Bash Automated Testing System).

```sh
# Install BATS (if needed)
sudo apt install bats  # Ubuntu/Debian
brew install bats-core # macOS

# Run all integration tests
./run_integration.sh

# Run a single test file
bats test/integration/allocator.bats

# Run tests matching a pattern
bats test/integration/allocator.bats -f "double-free"
```

**Performance Note**: Full integration tests take ~7 minutes to run. Only run `./run_integration.sh` before commits. During development, use targeted testing:
- `./run_one.sh <test_file>` - Test a single case quickly
- `bats test/integration/<file>.bats -f "pattern"` - Run specific tests by pattern
- `bats test/integration/<file>.bats` - Run one test file

**Note**: Integration tests are expected to fail during development (the CLR runtime is incomplete). However, if the tests fail due to **compilation errors in the emitted .air.zig analyzer**, that indicates a real problem in codegen that needs to be fixed.

**Important**: Always run BATS from the project root directory. The test helper uses relative paths from its location to find the compiler, libclr.so, and test cases.

**CRITICAL**: NEVER materially modify, comment out, or skip integration tests to make the test suite pass without permission. Failing tests are intentional - they serve as reminders of work to be done. If a test fails, fix the codegen or runtime - not the test. Minor updates like fixing line numbers after test file changes are fine. Integration tests should simply call `compile_and_run` and check the result.

**Important**: Do NOT report integration test failures as "pre-existing" unless you are CERTAIN the previous sprint left them as failures. Report failures without assuming they were already broken.

**Important**: Integration tests MUST have comprehensive output checking. For error-detecting tests, verify:
1. The exit status (`[ "$status" -ne 0 ]` for errors, `[ "$status" -eq 0 ]` for success)
2. The error message type (e.g., "use of undefined value found in")
3. The function name where the error occurred (e.g., "in function_name.main")
4. The file name and line/column of the error (e.g., "filename.zig:12:4)")
5. Any contextual messages (e.g., "undefined value assigned to 'x'")
6. The source location of the context (e.g., "filename.zig:2:4)")

Example of comprehensive test:
```bash
@test "detects undefined variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/basic/use_before_assign.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:3:4)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:2:4)" ]]
}
```

Test structure:
- `test/integration/*.bats` - BATS test files
- `test/integration/test_helper.bash` - Common setup and helper functions
- `test/cases/*.zig` - Test input files

### Manual Testing

```sh
# Build libclr first
zig build

# Run a single test case (NOT ./test.sh - that doesn't exist)
./run_one.sh test/cases/undefined/use_before_assign.zig

# Or manually:
zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air foo.zig
```

Output goes to stderr.

## Development Guidelines

- **Test-Driven Development (STRICT)** - Tests MUST exist and fail BEFORE writing implementation code:
  1. **New features**: Write integration tests FIRST, then unit tests for components
  2. **Bug fixes**: Write a failing test that reproduces the bug, then fix it
  3. **Never** propose or implement "testing" as a final phase - tests come first
  4. Follow the TDD Procedure below for adding new AIR tag handlers
- **Do not modify the zig/ submodule** - Any changes to the Zig compiler must be made by humans, not AI
- **Use debug.zig for debugging** - Use `src/debug.zig` for debug output. It uses `std.fmt.bufPrint` with raw Linux syscalls. Do not modify this file - it works correctly in the DLL context where `std.debug.print` does not.

## Iron Rules (DO NOT VIOLATE)

1. **testValid functions are IMMUTABLE** - The `testValid` functions in `lib/analysis/*.zig` define what analysis states are valid on which refinement types. These represent fundamental invariants of the analysis system. NEVER modify these functions to "fix" a test failure. If testValid is failing, the bug is in how refinements are being created or manipulated, not in testValid itself.

2. **Optional pointers are special** - In Zig, `?*T` (optional pointer) is the ONLY optional type with the same bit width as its non-optional form (null = address 0). This is why `bitcast` can convert `*T` to `?*T`. Other optionals have a separate null flag.

## Architecture: Tag Handlers vs Analysis Modules

**IMPORTANT**: Tag handlers in `lib/tag.zig` must NOT reach into analyte fields directly (e.g., `.analyte.undefined`, `.analyte.memory_safety`). Analyte manipulation is the responsibility of analysis modules in `lib/analysis/`.

Tag handlers should only:
1. Set up refinement STRUCTURE (pointer, scalar, struct, etc.)
2. Call `splat()` to dispatch to analysis modules
3. Use generic helpers like `copyAnalyteState()` that work on any analyte

If you find yourself writing `.analyte.undefined = ...` or `.analyte.memory_safety = ...` in a tag handler, that logic belongs in an analysis module instead.

## Zig Language Reminders

- **Flatten control flow** - Use Zig's tools to flatten imperative code instead of nesting logic. Early returns/continues with `orelse` and union checks keep the main logic at the top level:
  ```zig
  // BAD: Deeply nested logic
  for (results) |inst| {
      if (inst.memory_safety) |ms| {
          switch (ms) {
              .allocation => |a| {
                  if (a.freed == null) {
                      // ... actual logic buried 4 levels deep
                  }
              },
              .stack_ptr => {},
          }
      }
  }

  // GOOD: Flat logic with early continues
  for (results) |inst| {
      const ms = inst.memory_safety orelse continue;
      if (ms != .allocation) continue;
      const a = ms.allocation;
      if (a.freed != null) continue;
      // ... actual logic at top level
  }
  ```

- **Pointer capture for modification** - When you need to modify a value inside an optional, use `if (optional) |*ptr|` to capture a pointer. This is one case where nesting is appropriate:
  ```zig
  // Need to modify, so use pointer capture
  if (inst.memory_safety) |*ms| {
      if (ms.* != .allocation) continue;
      ms.allocation.freed = free_meta;  // modification requires pointer
  }
  ```

- **Avoid `&(x orelse y)`** - This pattern is hard to read and may not work as expected (takes address of temporary). Prefer `if` with pointer capture when modification is needed.

- **Panic on unexpected types, don't silently return** - In analysis handlers, if an operation expects a specific type (e.g., `store_safe` expects a pointer), panic on unexpected types rather than silently returning. This catches bugs early:
  ```zig
  // BAD: Silently ignores unexpected types
  const pointee_idx = switch (refinements.at(ptr_idx).*) {
      .pointer => |ind| ind.to,
      else => return,  // Bug hidden - we expected a pointer!
  };

  // GOOD: Panic on unexpected types - let Zig's union access panic naturally
  const pointee_idx = refinements.at(ptr_idx).pointer.to;
  ```

- **Always crash on `.unimplemented`** - Never silently handle or propagate `.unimplemented` refinements. If code encounters `.unimplemented`, it should crash immediately. This surfaces issues that need to be fixed (missing tag handlers, incomplete type tracking) rather than hiding them. The crash tells us exactly what needs to be implemented.

- **Rely on Zig's bounds checking** - Don't write explicit bounds checks before array access. Zig's runtime safety already panics on out-of-bounds access with a clear error message. Add a comment explaining why the access is guaranteed valid:
  ```zig
  // BAD: Redundant bounds check
  if (field_index >= s.fields.len) {
      std.debug.panic("out of bounds");
  }
  const field = s.fields[field_index];

  // GOOD: Let Zig handle it, add comment explaining guarantee
  // field_index comes from codegen which validates against struct type
  const field = s.fields[field_index];
  ```

- **Rely on Zig's union tag assertions** - Don't write explicit tag checks before accessing a union field. Accessing `union.field` panics on wrong tag in debug mode. Add a comment explaining why the tag is guaranteed:
  ```zig
  // BAD: Redundant tag check with switch
  switch (refinement.*) {
      .pointer => |p| doSomething(p),
      else => @panic("expected pointer"),
  }

  // BAD: Redundant tag check with if
  if (refinement.* != .pointer) @panic("expected pointer");
  const p = refinement.pointer;

  // GOOD: Let Zig handle it, add comment explaining guarantee
  // alloc always creates pointer refinement
  const p = refinement.pointer;
  ```

## TDD Procedure: Adding a New AIR Tag Handler

Follow these steps to add support for a new AIR instruction tag:

### 1. Write integration test FIRST (`test/integration/*.bats`)

Create the end-to-end test that will fail until implementation is complete:

```zig
// test/cases/my_feature/my_test.zig
pub fn main() u8 {
    // Zig code that exercises the new tag
}
```

```bash
# test/integration/my_feature.bats
@test "description of expected behavior" {
    run compile_and_run "$TEST_CASES/my_feature/my_test.zig"
    [ "$status" -eq 0 ]  # or check $output for expected errors
}
```

Run `./run_integration.sh` - the new test should FAIL. This confirms the test is valid.

### 2. Add codegen unit test (`src/codegen_test.zig`)

Write a test for the instruction line that should be generated:

```zig
test "instLine for my_new_tag" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ /* appropriate data for this tag */ };
    const result = codegen._instLine(arena.allocator(), dummy_ip, .my_new_tag, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Inst.apply(0, .{ .my_new_tag = .{ /* expected payload */ } }, results, ctx, &refinements);\n", result);
}
```

**Extra array format for `generateFunction` tests**: When testing `generateFunction`, the extra array must use `block_index` indirection:
- `extra[0]` = block_index (where the main body Block structure starts)
- `extra[block_index]` = body_len
- `extra[block_index + 1 ..]` = body instruction indices

Example for a 4-instruction function with main body [0, 1, 2, 3]:
```zig
const extra: []const u32 = &.{ 1, 4, 0, 1, 2, 3 };
//                              ^  ^  ^--------^ body indices
//                              |  body_len
//                              block_index
```

### 3. Add payload generator (`src/codegen.zig`)

Add the tag to the `payload()` switch and create a `payloadMyNewTag()` function:

```zig
fn payload(...) []const u8 {
    return switch (tag) {
        // ...existing tags...
        .my_new_tag => payloadMyNewTag(arena, datum),
        else => ".{}",
    };
}

fn payloadMyNewTag(arena: std.mem.Allocator, datum: Data) []const u8 {
    // Extract fields from datum and format the payload
    return clr_allocator.allocPrint(arena, ".{{ .field = {d} }}", .{datum.field}, null);
}
```

### 4. Add tag to AnyTag union (`lib/tag.zig`)

Tags are defined inline in `lib/tag.zig`. Add a new struct type and include it in the `AnyTag` union:

```zig
// Define the tag struct (in lib/tag.zig)
pub const MyNewTag = struct {
    field: u32,  // fields matching the payload

    pub fn apply(self: @This(), results: []Inst, index: usize, ctx: *Context, refinements: *Refinements) !void {
        // Tag-specific logic (if any) before analysis dispatch
        try splat(.my_new_tag, results, index, ctx, refinements, self);
    }
};

pub const AnyTag = union(enum) {
    // ...existing tags...
    my_new_tag: MyNewTag,
};
```

### 5. Add analysis handler (if needed) (`lib/analysis/*.zig`)

If the tag affects an analysis (e.g., undefined tracking), add a handler:

```zig
// In lib/analysis/undefined.zig (or other analysis module)
pub fn my_new_tag(results: []Inst, index: usize, ctx: *Context, refinements: *Refinements, payload: anytype) !void {
    // Analysis logic - update results, report errors, etc.
}
```

The `splat()` function in `lib/tag.zig` uses `@hasDecl` to automatically dispatch to any analysis that implements the tag.

### 6. Run unit tests

```sh
zig build test
```

### 7. Verify integration tests pass

```sh
./run_integration.sh
```

The tests from step 1 should now pass.

### Key Files Reference

| File | Purpose |
|------|---------|
| `src/codegen.zig` | Generates .air.zig source from AIR instructions |
| `src/codegen_test.zig` | Unit tests for codegen |
| `lib/tag.zig` | AnyTag union, tag structs, and splat dispatch |
| `lib/analysis/*.zig` | Analysis modules (undefined, memory_safety) |
| `lib/Inst.zig` | Inst struct, results list management, and entity helpers |
| `lib/Refinements.zig` | Refinements table and Refinement union |
| `test/cases/` | Integration test input files |
| `test/integration/*.bats` | BATS integration tests |

## DLL Relocation Issues (IMPORTANT)

libclr.so is dynamically loaded via `dlopen()`. This causes a specific class of failures:

**Problem**: Compile-time initialized vtables with function pointers don't get properly relocated when loaded as a shared library. The pointers end up pointing to invalid addresses (e.g., `0x8`, `0x118b2`).

**Affected std library features** (known failures):
- `std.heap.page_allocator` - crashes
- `std.heap.ArenaAllocator` - crashes (static vtable)
- `std.fmt.allocPrint` - crashes (uses ArrayList with Writer vtable)
- `std.fmt.count` - crashes (uses Writer.Discarding vtable)
- `std.debug.print` - crashes (uses stderr writer vtable)

**Debugging approach**:
1. **Try std library functions first** - they may work
2. **If a segfault occurs**, inspect the std library source code to check if the function uses compile-time initialized vtables or function pointer tables
3. **If it's a const vtable relocation issue**, implement a workaround:
   - Use `std.fmt.bufPrint` instead of allocPrint (no vtable)
   - Initialize vtables at runtime in `init()` instead of compile-time
   - Use raw Linux syscalls for I/O (see `debug.zig`)
   - Accept allocators from the host process via C-ABI vtables

**Solution pattern**: See `src/allocator.zig` for the canonical workaround - accept an AllocatorVTable from the host (with C-ABI shims), and initialize all local vtables at runtime.

**Critical implementation details**:
1. **Vtables must be module-level `var`** - If you put a vtable inside a struct, even if you initialize it at runtime in `init()`, the struct gets copied by value and the function pointers become invalid. Use a single shared module-level `var vtable` that's initialized once in the global `init()`.

2. **Use `clr_allocator.allocPrint` instead of `std.fmt.allocPrint`** - The stdlib version uses internal Writer vtables that crash. Our version uses `bufPrint` which has no vtables.

3. **`allocPrint` needs size hints for large strings** - When formatting a large string (e.g., 18KB+ of inst lines) into a small buffer, `bufPrint` can crash instead of returning `NoSpaceLeft`. Pass a `size_hint` parameter when you know the approximate output size: `clr_allocator.allocPrint(alloc, fmt, args, size_hint)`. Note: Complex generics (like `GeneralPurposeAllocator`) have very long FQNs (hundreds of characters), so use generous margins (4096+ bytes) for function templates.

4. **Per-function arenas must share the global vtable** - When creating temporary arenas (e.g., for building inst lines), use `clr_allocator.newArena()` which returns an Arena that uses the shared `arena_vtable`.

5. **Extern union field access can crash** - Accessing fields of `extern union` types (like `Air.Inst.Data`) can crash in DLL context, even when the data is valid and bounds checks pass. The workaround is to read raw bytes and reinterpret manually:
   ```zig
   // CRASHES in DLL context:
   const ty_op = datum.ty_op;

   // WORKS - read raw bytes instead:
   const raw_ptr: [*]const u32 = @ptrCast(&datum);
   const ty_raw: u32 = raw_ptr[0];
   const ty_ref: Ref = @enumFromInt(ty_raw);
   ```
   This pattern is used in `src/codegen.zig` in `getInstResultType()` when reading type information from AIR instruction data.

## Interprocedural Analysis Architecture

### AIR Arg Semantics (IMPORTANT)

AIR arg instructions contain VALUES directly, NOT pointers to stack locations:

- If the parameter type is `*u8`, the instruction contains the pointer value itself
- If the parameter type is `u8`, the instruction contains the scalar value itself
- Taking `&param` in source code generates explicit `alloc` + `store_safe` in AIR

This means the Arg handler does NOT wrap entities in a pointer. It copies the caller's entity directly into local refinements.

**Example**: For `fn set_value(ptr: *u8) { ptr.* = 5; }`:
1. Caller passes pointer entity P1 → scalar S1 (undefined)
2. Arg copies P1 → P1' and S1 → S1' in local refinements
3. Inst 0 contains P1' directly (the pointer entity, NOT a pointer to it)
4. `store_safe(ptr=0)` follows P1' to S1', marks S1' as defined
5. `backPropagate` propagates S1'.undefined back to S1.undefined

### Local-Only Updates, Propagate on Close

When a callee modifies state through a pointer argument, we do NOT update the caller's state directly during the operation. Instead:

1. **Arg handler**: Copies the caller's entity into local refinements (deep copy via copyEntityRecursive). Sets `caller_ref` for backward propagation.
2. **During execution**: All operations (store_safe, load, etc.) only update the LOCAL copy
3. **On function close**: `backPropagate` propagates final state back to the caller via `caller_ref`

**Why this architecture?**

The callee's results table must remain "clean" during execution. As we implement conditionals and loops, we'll need to merge multiple copies of results tables (e.g., merging the "then" and "else" branches of an if statement). If operations directly modified the caller's state, we'd be making partial updates that couldn't be cleanly merged.

By deferring propagation until function close, we ensure:
- Each branch maintains its own complete view of the state
- Merging happens on fully-resolved states, not partial updates
- The caller only sees the final result after all paths are considered

### Inst.caller_ref

For arguments, `Inst.caller_ref` stores the caller's entity index (EIdx).

This allows `backPropagate` to find the caller's entity, follow it (if it's a pointer), and update the final state.

## Not going to do

### Inst Type Tracking

**Issue**: Currently, instructions don't and should not track what type of value they contain.
- Type safety is the responsibility of the compiler so all generated AIR should already
  conform to type correctness
- Const correctness is enforced at the compiler level, so any AIR operation that might
  clobber a const variable or a pointer-to-const simply shouldn't happen.
- Type flexibility is important.  If you are for example converting a pointer to an integer
  it is useful to continue to track that integer *as if it were a pointer*.  Though this
  is not supported at the moment.

### Invalid union tag detection

We won't prevent failures due to union tag field being set to an invalid value. In release safe modes this will cause a runtime crash, or will exhibit undefined behavior in optimized builds.

## Known Limitations

### Load creates fresh scalar (breaks `**T` tracking)

The `Load` tag currently always creates a fresh scalar entity for its result. This is incorrect for pointer-to-pointer types like `**u8`. When loading from a `**u8`, we should get the inner pointer entity, not a fresh scalar.

**Impact**: Multi-level pointer dereferences lose tracking. For example:
```zig
var x: u8 = undefined;
var ptr: *u8 = &x;
var ptr_ptr: **u8 = &ptr;
var inner = ptr_ptr.*;  // Load here creates fresh scalar, loses pointer tracking
inner.* = 5;  // This store won't be tracked back to x
```

**Fix**: Load should follow the pointer and share the pointee's entity instead of creating a fresh scalar.

**Test needed**: `test/cases/undefined/double_pointer.zig`

### Setting a pointer to undefined

Need to investigate what happens when a pointer itself is set to undefined (e.g., `var ptr: *u8 = undefined;`). Currently we set `pointer.analyte.undefined = undef_state`, but this needs testing and verification.

We may need to make `pointer.to` an optional value - an undefined pointer doesn't point to anything valid.

### Simple tags need BinOp/UnOp refinement

The `Simple` type currently just produces a scalar without tracking operands. For proper data flow analysis, we'll need to:
- Split into `BinOp` (binary operations like `bit_and`, `cmp_*`, `sub`) and `UnOp` (unary operations like `ctz`)
- Pass operand parameters to track where values flow from
- Currently affects: `bit_and`, `cmp_eq`, `cmp_gt`, `cmp_lte`, `ctz`, `sub`

### Runtime union tag comparisons not tracked

Variant safety only recognizes the pattern `if (some_union == .variant_name)` where `.variant_name` is a comptime-known enum literal. Comparisons against runtime union tags are not tracked:

```zig
const some_tag: SomeUnion = get_tag_at_runtime();
if (my_union == some_tag) {  // NOT recognized - some_tag is runtime value
    _ = my_union.field;  // May incorrectly report inactive variant access
}
```

This is an intentional limitation - tracking runtime tag values would require complex value flow analysis.

### `dbg_inline_block` not implemented

The `dbg_inline_block` instruction is used for inlined function calls. This needs investigation to understand how it affects data flow tracking - inlined code may need special handling to maintain correct interprocedural analysis.

### Other return instructions not implemented

Only `ret_safe` is implemented. The following return variants are marked as `Unimplemented` and need proper handling:

- **`ret_addr`**: Returns the return address (used for stack traces). Probably safe to leave as unimplemented since it's metadata, not data flow.
- **`ret_load`**: Returns by loading from a pointer (used for large return values that don't fit in registers). **TODO**: This affects data flow and needs implementation.
- **`ret_ptr`**: Returns the pointer where the return value should be stored (caller provides storage). **TODO**: This affects data flow and needs implementation.

## Known Issues / Future Work

**InternPool type inspection**: See `zig/src/InternPool.zig` for `indexToKey()` which returns type information that can be pattern-matched to determine the category.

**Returned pointers shouldn't trigger leak detection**: When a function returns a pointer, the local leak detection (in `onFinish`) incorrectly reports a leak because the allocation is "not freed" in the returning function. The caller takes ownership, so the return should exempt the pointer from local leak checks.

## AIR Backend Overview

The project adds a custom AIR (Abstract Intermediate Representation) output backend to Zig:

- `-fair-out=<path>` flag routes AIR to a dynamically loaded .so plugin
- Object format: `-ofmt=air`
- Key files in the zig submodule:
  - `src/codegen/air.zig` - Codegen module with `generate` function
  - `src/link/Air.zig` - Linker integration
  - `src/Compilation/Config.zig` - `air_out` configuration field

## Accessing Function Info in Codegen

In `src/codegen/air.zig`, to get function details from `func_index`:

```zig
const zcu = pt.zcu;
const ip = &zcu.intern_pool;
const func = zcu.funcInfo(func_index);
const nav = ip.getNav(func.owner_nav);
const name = nav.name.toSlice(ip);  // function name as []const u8
const fqn = nav.fqn.toSlice(ip);    // fully qualified name
```

## Allocator Type Identification

When tracking allocator create/destroy for mismatched allocator detection, we need to identify which allocator TYPE is being used.

### Comptime Allocators (e.g., `std.heap.page_allocator`)

For comptime-known allocators, the allocator argument to `create`/`destroy` is an **interned aggregate**:

```zig
// In call args, arg[0] may be interned
if (arg_ref.toInterned()) |ip_idx| {
    const arg_key = ip.indexToKey(ip_idx);
    switch (arg_key) {
        .aggregate => |agg| {
            // Allocator struct has 2 fields: ptr (undef) and vtable
            const elems = agg.storage.elems;
            const vtable_elem = elems[1];  // vtable pointer
            const vtable_key = ip.indexToKey(vtable_elem);
            switch (vtable_key) {
                .ptr => |ptr| {
                    switch (ptr.base_addr) {
                        .nav => |nav_idx| {
                            const nav = ip.getNav(nav_idx);
                            // nav.fqn gives "heap.PageAllocator.vtable"
                            // Extract "PageAllocator" to identify allocator type
                        },
                        else => {},
                    }
                },
                else => {},
            }
        },
        else => {},
    }
}
```

**Key insight**: The vtable pointer's nav FQN (e.g., `heap.PageAllocator.vtable`) uniquely identifies the allocator type.

### Runtime Allocators (e.g., `gpa.allocator()`)

For runtime allocators, the argument is an **inst_ref** pointing to an AIR instruction:

```zig
if (arg_ref.toIndex()) |inst_idx| {
    // inst_idx points to an AIR instruction that produces the Allocator value
    // Need to trace back through AIR to find the source
}
```

To identify runtime allocator types, trace the inst_ref back through the AIR:
1. The inst_ref points to a `load` or `call` instruction
2. For `gpa.allocator()`, trace to the call to `.allocator()` method
3. The receiver type of that call (e.g., `GeneralPurposeAllocator`) identifies the allocator type

**Current Status**:
- Comptime allocators (e.g., `std.heap.page_allocator`) work - we extract "PageAllocator" from the vtable FQN
- Runtime allocators currently return "Allocator" (generic marker)

**Future Improvements**:
1. **Global/const allocator tracking**: If an allocator is stored in a global or const variable, trace through the store to find the vtable source
2. **Allocator type tracking**: Track allocator types through the inst typing system - when a variable is declared as a specific allocator type (e.g., `var gpa = GeneralPurposeAllocator(.{}){}`), propagate that type label to any `Allocator` derived from it via `.allocator()`
3. **Vtable field tracing**: Search for `struct_field_ptr_index_1` (vtable field) stores to find the vtable global for runtime-constructed Allocator structs