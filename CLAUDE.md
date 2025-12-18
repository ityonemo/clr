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

# Run integration tests
./run_integration.sh
```

**Note**: Integration tests are expected to fail during development (the CLR runtime is incomplete). However, if the tests fail due to **compilation errors in the emitted .air.zig analyzer**, that indicates a real problem in codegen that needs to be fixed.

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

- **Pointer capture for optionals** - When you need to modify a value inside an optional, use `if (optional) |*ptr|` to capture a pointer to the payload:
  ```zig
  // Instead of this awkward pattern:
  const meta = &(tracked[slot].memory_safety orelse return).stack_ptr;
  meta.name = .{ .variable = name };

  // Use pointer capture:
  if (tracked[slot].memory_safety) |*ms| {
      ms.stack_ptr.name = .{ .variable = name };
  }
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
    const result = codegen._slotLine(arena.allocator(), dummy_ip, .my_new_tag, datum, 0, &.{}, &.{}, &.{});

    try std.testing.expectEqualStrings("    try Slot.apply(.{ .my_new_tag = .{ /* expected payload */ } }, tracked, 0, ctx);\n", result);
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

```zig
pub const AnyTag = union(enum) {
    // ...existing tags...
    my_new_tag: @import("tag/MyNewTag.zig"),
};
```

### 5. Create tag handler (`lib/tag/MyNewTag.zig`)

```zig
const Slot = @import("../slots.zig").Slot;
const splat = @import("../tag.zig").splat;

field: u32,  // fields matching the payload

pub fn apply(self: @This(), tracked: []Slot, index: usize, ctx: anytype) !void {
    // Tag-specific logic (if any) before analysis dispatch
    try splat(.my_new_tag, tracked, index, ctx, self);
}
```

### 6. Add analysis handler (if needed) (`lib/analysis/*.zig`)

If the tag affects an analysis (e.g., undefined tracking), add a handler:

```zig
// In lib/analysis/undefined.zig (or other analysis module)
pub fn my_new_tag(tracked: []Slot, index: usize, ctx: anytype, payload: anytype) !void {
    // Analysis logic - update tracked slots, report errors, etc.
}
```

The `splat()` function in `lib/tag.zig` uses `@hasDecl` to automatically dispatch to any analysis that implements the tag.

### 7. Run unit tests

```sh
zig build test
```

### 8. Verify integration tests pass

```sh
./run_integration.sh
```

The tests from step 1 should now pass.

### Key Files Reference

| File | Purpose |
|------|---------|
| `src/codegen.zig` | Generates .air.zig source from AIR instructions |
| `src/codegen_test.zig` | Unit tests for codegen |
| `lib/tag.zig` | AnyTag union and splat dispatch |
| `lib/tag/*.zig` | Individual tag handlers |
| `lib/analysis/*.zig` | Analysis modules (undefined, etc.) |
| `lib/slots.zig` | Slot struct and apply/call dispatch |
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

3. **`allocPrint` needs size hints for large strings** - When formatting a large string (e.g., 18KB+ of slot lines) into a small buffer, `bufPrint` can crash instead of returning `NoSpaceLeft`. Pass a `size_hint` parameter when you know the approximate output size: `clr_allocator.allocPrint(alloc, fmt, args, size_hint)`.

4. **Per-function arenas must share the global vtable** - When creating temporary arenas (e.g., for building slot lines), use `clr_allocator.newArena()` which returns an Arena that uses the shared `arena_vtable`.

## Known Issues / Future Work

### Const vs Mutable Pointer Parameter Tracking

**Issue**: The `arg_ptr` mechanism propagates analysis information backwards through pointer parameters to update the caller's slot state. Currently, this happens for ALL pointer parameters, but it should NOT happen for `*const` pointers since the callee cannot modify through them.

**Current behavior** (in `lib/tag/Arg.zig`):
```zig
tracked[index].arg_ptr = self.value;  // Always set, regardless of const
```

**Problem**: In `lib/analysis/undefined.zig`'s `store_safe`, we propagate backwards unconditionally:
```zig
if (tracked[ptr].arg_ptr) |caller_slot| {
    caller_slot.undefined = tracked[ptr].undefined;  // Wrong for *const!
}
```

This could cause incorrect interprocedural analysis - information flowing backwards through const pointers when it shouldn't.

**Fix needed**:
1. In `src/codegen.zig`'s `payloadArg()`, check if `datum.arg.ty` is a const pointer type
2. Add `is_const: bool` field to the arg payload
3. In `lib/tag/Arg.zig`, only set `arg_ptr` when `!self.is_const`

**Type checking approach**: Use the InternPool to check if the parameter type is a pointer with const pointee.

### Slot Type Tracking

**Issue**: Currently, slots don't track what type of value they contain. This makes it difficult to reason about:
- Whether a slot holds a pointer vs a value type
- Whether a pointer is const or mutable
- Struct vs slice vs primitive types
- How to correctly propagate analysis information through different type categories

**Why this matters**:
- Pointer types need different handling than value types (e.g., `&param` creates new stack memory)
- Const pointers shouldn't allow backward propagation
- Slices contain both a pointer and a length - the pointer part may escape but the length is a value
- Structs may contain pointers that need tracking

**Proposed approach**:
1. Add a `type_info` field to `Slot` that captures relevant type categories
2. During codegen, extract type information from AIR's `datum.*.ty` fields using InternPool
3. Include type category in the generated payload (e.g., `.{ .value = arg0, .name = "x", .type = .ptr_const }`)
4. Use type info in analysis handlers to make correct decisions about propagation

**Type categories to consider**:
- `.value` - primitive value types (integers, floats, bool, etc.)
- `.ptr_const` - pointer to const data
- `.ptr_mut` - pointer to mutable data
- `.slice` - slice type (ptr + len)
- `.struct_val` - struct by value
- `.union_val` - union by value
- `.optional` - optional types
- `.array` - fixed-size arrays

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