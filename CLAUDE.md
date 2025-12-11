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

Test structure:
- `test/integration/*.bats` - BATS test files
- `test/integration/test_helper.bash` - Common setup and helper functions
- `test/cases/*.zig` - Test input files

### Manual Testing

```sh
# Build libclr first
zig build

# Run the custom compiler with the AIR backend on foo.zig
./test.sh

# Or manually:
zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air foo.zig
```

The test file `foo.zig` contains a simple main function. Output goes to stderr.

## Development Guidelines

- **Test-Driven Development** - Write tests first, then implementation
- **Do not modify the zig/ submodule** - Any changes to the Zig compiler must be made by humans, not AI
- **Use debug.zig for debugging** - Use `src/debug.zig` for debug output. It uses `std.fmt.bufPrint` with raw Linux syscalls. Do not modify this file - it works correctly in the DLL context where `std.debug.print` does not.

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