# CLR

**C**hecker of

**L**ifetimes and other

**R**efinement types

for Zig

Video: https://www.youtube.com/watch?v=mf0WzTOe-40
Sponsorship: https://buymeacoffee.com/dnautics

discuss on hn: https://news.ycombinator.com/item?id=42923829

discuss on lobste.rs: https://lobste.rs/s/9sitsj/clr_checker_for_lifetimes_other

live demo video: https://www.youtube.com/watch?v=ZY_Z-aGbYm8

## Overview

This project creates a Zig transpiler for the Zig compiler, which transforms AIR (Abstract Intermediate Representation) into Zig source code that performs static analysis at compile time. The generated analyzer will be able to catche memory safety issues like use-before-assignment, use-after-free, stack pointer escapes,
as well as zig-specific UB, such as non-nullness assertions, (un)tagged union violations, or fieldParentPtr misuse.

The goal is to bring Rust-level memory safety guarantees to Zig through static analysis of AIR, without changing the language itself.

CLR depends on a forked version of the Zig compiler (included as a submodule in `zig/`) that adds support for routing AIR to external plugins. When invoked with `-ofmt=air -fair-out=<plugin.so>`, the compiler loads the specified shared library and passes generated AIR to it for processing.

## Status

This is an active rewrite of the original Elixir-based proof-of-concept in Zig. The Zig implementation loads as a compiler plugin and analyzes AIR directly.

Currently implemented:
- Undefined value tracking (use before assignment, field-level tracking for structs)
- Memory safety analysis (use-after-free, double-free, memory leaks)
- Allocator mismatch detection (freeing with wrong allocator)
- Null safety (unchecked optional unwrap detection)
- Interprocedural analysis (tracking values across function calls via pointer arguments)
- Struct field tracking (pointer fields, nested structs)
- Source location and variable name tracking for error messages
- Branching/control flow with state merging

Planned (see LIMITATIONS.md for details):
- Union tracking
- Loop analysis (fixed-point iteration)
- Switch statement support (n-way merging)
- Large return value tracking (ret_load, ret_ptr)

## Prerequisites

- Zig 0.15.2 (system installation with LLVM support)
- Linux (currently Linux-only due to direct syscall usage)
- BATS (for integration tests): `sudo apt install bats`

## Building

The vendored Zig compiler and libclr plugin must be built with matching optimization levels.
Mismatched optimization levels will cause segfaults.

```sh
# Build the custom Zig compiler with ReleaseFast (first time only, or after submodule changes)
cd zig && zig build --zig-lib-dir lib -Doptimize=ReleaseFast && cd ..

# Build the CLR plugin with matching optimization
zig build -Doptimize=ReleaseFast
```

For development/debugging, use ReleaseSafe or Debug for both:

```sh
# ReleaseSafe (with safety checks, slightly slower)
cd zig && zig build --zig-lib-dir lib -Doptimize=ReleaseSafe && cd ..
zig build -Doptimize=ReleaseSafe

# Debug (full debug info, slowest)
cd zig && zig build --zig-lib-dir lib && cd ..
zig build
```

## Usage

```sh
# Compile a Zig file using the AIR backend
zig/zig-out/bin/zig build-exe -fair-out=zig-out/lib/libclr.so -ofmt=air -femit-bin=output.air.zig your_file.zig

# Run the generated analyzer
zig run --dep clr -Mroot=output.air.zig -Mclr=lib/lib.zig
```

Output goes to stderr.

## Testing

```sh
# Unit tests (codegen/DLL)
zig build test

# Unit tests (runtime library)
zig test lib/lib.zig

# Integration tests (requires BATS)
# Defaults to ReleaseFast; override with OPTIMIZE=ReleaseSafe or OPTIMIZE=Debug
./run_integration.sh

# Manual test of a single file
./run_one.sh test/cases/undefined/use_before_assign.zig
```

**Note:** Integration tests rebuild libclr with the specified optimization level (default: ReleaseFast).
Make sure your vendored Zig compiler was built with a matching optimization level.

## Project Structure

```
clr/
├── src/                     # DLL/plugin code (generates .air.zig)
│   ├── clr.zig              # Main CLR plugin entry point
│   ├── codegen.zig          # Generates .air.zig source from AIR instructions
│   └── allocator.zig        # DLL-safe allocator wrapper
├── lib/                     # Runtime analysis library
│   ├── lib.zig              # Library entry point
│   ├── tag.zig              # AnyTag union, Type, tag handlers, splat dispatch
│   ├── Inst.zig             # Instruction results and interprocedural analysis
│   ├── Refinements.zig      # Refinement types (pointer, struct, optional, etc.)
│   ├── Analyte.zig          # Analysis state container
│   ├── Context.zig          # Execution context (metadata, error reporting)
│   └── analysis/            # Analysis modules
│       ├── undefined.zig    # Use-before-assign tracking
│       ├── memory_safety.zig # Allocation/free tracking
│       └── null_safety.zig  # Optional unwrap checking
├── test/
│   ├── integration/         # BATS integration tests
│   │   ├── test_helper.bash
│   │   └── *.bats
│   └── cases/               # Test input files (.zig)
├── zig/                     # Zig compiler submodule (instrumented fork)
├── build.zig                # Build configuration
└── build.zig.zon            # Package dependencies
```

## The General Idea

![diagram](./images/diagram.png)

Zig is a famously "unsafe" language. Memory management is done manually, which opens up the possibility of implementation errors. While Zig reduces security issues relative to C by eliminating out-of-bounds array access and null pointer dereferencing in safety-checked code, it is still less safe than Rust, which eliminates use-after-free, double-free, and data races through static analysis.

Inspired by Rust's MIRI project, CLR performs static analysis on Zig's AIR intermediate representation to achieve a higher degree of safety than Zig provides out-of-the-box. Unlike MIRI, which interprets Rust's MIR in a sandboxed pseudo-runtime, CLR transpiles AIR into Zig source code that statically runs analysis.  Note that CLR's air output zig code could in principle be run at compile time, but by running through a zig intermediate, we produce an easy-to-understand and easy-to-debug logical flow.  An ambitious person might use this general approach to emit a different output target, such as a proof assistant language, or refactor it to run entirely in the zig compiler!

The key insight: if you need MIRI for security-conscious Rust projects anyway, why not pick a simpler language and do MIRI-style analysis to get borrow checking and other refinement-type analysis? This project shows that such a future is a real possibility for Zig.

The Zig compilation pipeline is:

1. Zig Code / AST
2. Zig ZIR (untyped, per-file intermediate representation)
3. Zig AIR (typed, per-function IR, reified for polymorphic functions)
4. Compiler targets

AIR is the ideal level for analysis because it's typed, interpretable as a "minimum viable" list of generalized programming instructions, and allows extending types with refinement metadata.

For an in-depth look at how Zig's AIR works, see Mitchell Hashimoto's blog post: https://mitchellh.com/zig/sema

## License

MIT License - see [LICENSE](LICENSE) for details.
