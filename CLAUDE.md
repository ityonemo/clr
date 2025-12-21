# CLR Project

## Project Structure

- `zig/` - Zig compiler submodule (DO NOT MODIFY - all changes must be non-AI)

## Build Commands

- `zig/b` - Builds the custom Zig compiler (uses `zig build --zig-lib-dir lib`)
- `zig/z` - Runs the custom Zig compiler from `zig-out/bin/zig`
- `zig build` - Builds libclr.so (the AIR plugin)
- `zig build test` - Runs unit tests for libclr
- `./run_integration.sh` - Runs BATS integration tests

## Testing

### Unit Tests

```sh
zig build test
```

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

**Note**: Integration tests are expected to fail during development (the CLR runtime is incomplete). However, if the tests fail due to **compilation errors in the emitted .air.zig analyzer**, that indicates a real problem in codegen that needs to be fixed.

**Important**: Always run BATS from the project root directory. The test helper uses relative paths from its location to find the compiler, libclr.so, and test cases.

**Important**: Do NOT modify integration tests to make them pass, unless we have decided to materially change the shape of the output. Integration tests should simply call `compile_and_run` and check the result. If a test fails, fix the codegen or runtime - not the test.

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

## Zig Language Reminders

- **Flatten control flow** - Use Zig's tools to flatten imperative code instead of nesting logic. Early returns/continues with `orelse` and union checks keep the main logic at the top level:
  ```zig
  // BAD: Deeply nested logic
  for (tracked) |slot| {
      if (slot.memory_safety) |ms| {
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
  for (tracked) |slot| {
      const ms = slot.memory_safety orelse continue;
      if (ms != .allocation) continue;
      const a = ms.allocation;
      if (a.freed != null) continue;
      // ... actual logic at top level
  }
  ```

- **Pointer capture for modification** - When you need to modify a value inside an optional, use `if (optional) |*ptr|` to capture a pointer. This is one case where nesting is appropriate:
  ```zig
  // Need to modify, so use pointer capture
  if (slot.memory_safety) |*ms| {
      if (ms.* != .allocation) continue;
      ms.allocation.freed = free_meta;  // modification requires pointer
  }
  ```

- **Avoid `&(x orelse y)`** - This pattern is hard to read and may not work as expected (takes address of temporary). Prefer `if` with pointer capture when modification is needed.

- **Panic on unexpected types, don't silently return** - In analysis handlers, if an operation expects a specific type (e.g., `store_safe` expects a pointer), panic on unexpected types rather than silently returning. This catches bugs early:
  ```zig
  // BAD: Silently ignores unexpected types
  const pointee_idx = switch (payloads.at(ptr_idx).*) {
      .pointer => |ind| ind.to,
      else => return,  // Bug hidden - we expected a pointer!
  };

  // GOOD: Panic on unexpected types, explicit handling for known cases
  const pointee_idx = switch (payloads.at(ptr_idx).*) {
      .pointer => |ind| ind.to,
      .unimplemented => return,  // TODO: explicit handling
      else => |t| std.debug.panic("store_safe: expected pointer, got {s}", .{@tagName(t)}),
  };
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

Write a test for the slot line that should be generated:

```zig
test "slotLine for my_new_tag" {
    initTestAllocator();
    defer clr_allocator.deinit();

    var arena = clr_allocator.newArena();
    defer arena.deinit();

    const datum: Data = .{ /* appropriate data for this tag */ };
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .my_new_tag, datum, 0, &.{}, &.{}, &.{}, &.{}, null);

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .my_new_tag = .{ /* expected payload */ } }, tracked, 0, ctx, &payloads);\n", result);
}
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

    pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype, payloads: *Payloads) !void {
        // Tag-specific logic (if any) before analysis dispatch
        try splat(.my_new_tag, tracked, index, ctx, payloads, self);
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
pub fn my_new_tag(tracked: []Slot, index: usize, ctx: anytype, payloads: *Payloads, payload: anytype) !void {
    // Analysis logic - update tracked slots, report errors, etc.
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
| `lib/slots.zig` | Slot struct, Payloads, and entity system |
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

3. **`allocPrint` needs size hints for large strings** - When formatting a large string (e.g., 18KB+ of slot lines) into a small buffer, `bufPrint` can crash instead of returning `NoSpaceLeft`. Pass a `size_hint` parameter when you know the approximate output size: `clr_allocator.allocPrint(alloc, fmt, args, size_hint)`. Note: Complex generics (like `GeneralPurposeAllocator`) have very long FQNs (hundreds of characters), so use generous margins (4096+ bytes) for function templates.

4. **Per-function arenas must share the global vtable** - When creating temporary arenas (e.g., for building slot lines), use `clr_allocator.newArena()` which returns an Arena that uses the shared `arena_vtable`.

## Interprocedural Analysis Architecture

### AIR Arg Semantics (IMPORTANT)

AIR arg slots contain VALUES directly, NOT pointers to stack locations:

- If the parameter type is `*u8`, the slot contains the pointer value itself
- If the parameter type is `u8`, the slot contains the scalar value itself
- Taking `&param` in source code generates explicit `alloc` + `store_safe` in AIR

This means the Arg handler does NOT wrap entities in a pointer. It copies the caller's entity directly into local payloads.

**Example**: For `fn set_value(ptr: *u8) { ptr.* = 5; }`:
1. Caller passes pointer entity P1 → scalar S1 (undefined)
2. Arg copies P1 → P1' and S1 → S1' in local payloads
3. Slot 0 contains P1' directly (the pointer entity, NOT a pointer to it)
4. `store_safe(ptr=0)` follows P1' to S1', marks S1' as defined
5. `onFinish` propagates S1'.undefined back to S1.undefined

### Local-Only Updates, Propagate on Close

When a callee modifies state through a pointer argument, we do NOT update the caller's state directly during the operation. Instead:

1. **Arg handler**: Copies the caller's entity into local payloads (deep copy via copyEntityRecursive). Sets `caller_ref` for backward propagation.
2. **During execution**: All operations (store_safe, load, etc.) only update the LOCAL copy
3. **On function close**: `onFinish` propagates final state back to the caller via `caller_ref`

**Why this architecture?**

The callee's slot table must remain "clean" during execution. As we implement conditionals and loops, we'll need to merge multiple copies of slot tables (e.g., merging the "then" and "else" branches of an if statement). If operations directly modified the caller's state, we'd be making partial updates that couldn't be cleanly merged.

By deferring propagation until function close, we ensure:
- Each branch maintains its own complete view of the state
- Merging happens on fully-resolved states, not partial updates
- The caller only sees the final result after all paths are considered

### Slot.caller_ref

For arguments, `Slot.caller_ref` stores:
- `payloads`: Pointer to the caller's Payloads
- `entity_idx`: The caller's entity index (EIdx)

This allows `onFinish` to find the caller's entity, follow it (if it's a pointer), and update the final state.

## Not going to do

### Slot Type Tracking

**Issue**: Currently, slots don't and should not track what type of value they contain. 
- Type safety is the responsibility of the compiler so all generated AIR should already
  conform to type correctness
- Const correctness is enforced at the compiler level, so any AIR operation that might
  clobber a const variable or a pointer-to-const simply shouldn't happen.
- Type flexibility is important.  If you are for example converting a pointer to an integer
  it is useful to continue to track that integer *as if it were a pointer*.  Though this
  is not supported at the moment.

## Known Issues / Future Work

**InternPool type inspection**: See `zig/src/InternPool.zig` for `indexToKey()` which returns type information that can be pattern-matched to determine the category.

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
2. **Allocator type tracking**: Track allocator types through the slot typing system - when a variable is declared as a specific allocator type (e.g., `var gpa = GeneralPurposeAllocator(.{}){}`), propagate that type label to any `Allocator` derived from it via `.allocator()`
3. **Vtable field tracing**: Search for `struct_field_ptr_index_1` (vtable field) stores to find the vtable global for runtime-constructed Allocator structs