# CLR - Common Language Runtime for Zig

CLR is a plugin for the Zig compiler that intercepts AIR (Abstract Intermediate Representation) and generates alternative output formats.

## Overview

This project extends the Zig compiler with a custom AIR backend that can be dynamically loaded at compile time. When invoked with `-ofmt=air -fair-out=<plugin.so>`, the compiler routes generated AIR through the specified shared library instead of the standard code generation pipeline.

## Prerequisites

- Zig 0.15.2 (system installation with LLVM support)
- Linux (currently Linux-only due to direct syscall usage)
- BATS (for integration tests): `sudo apt install bats`

## Building

```sh
# Build the custom Zig compiler (first time only, or after submodule changes)
cd zig && ./b && cd ..

# Build the CLR plugin
zig build
```

## Usage

```sh
# Compile a Zig file using the AIR backend
zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air your_file.zig
```

## Testing

```sh
# Unit tests
zig build test

# Integration tests (requires BATS)
./run_integration.sh

# Quick manual test
./test.sh
```

## Project Structure

```
clr/
├── src/
│   └── clr.zig          # Main CLR plugin implementation
├── test/
│   ├── integration/     # BATS integration tests
│   │   ├── test_helper.bash
│   │   └── *.bats
│   └── cases/           # Test input files
├── zig/                 # Zig compiler submodule (instrumented fork)
├── build.zig            # Build configuration
└── build.zig.zon        # Package dependencies
```

## Plugin Interface

The CLR plugin exports the following functions:

- `init() u8` - Called once when the plugin is loaded. Returns 0 on success.
- `generate(pt, air, src_loc, func_index) *anyopaque` - Called for each function during codegen.
- `mir_deinit(result) void` - Called to clean up results from generate.
- `deinit() void` - (Optional) Called when the plugin is unloaded.

## License

[TODO: Add license]
