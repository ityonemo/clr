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

This project creates a Zig transpiler for the Zig compiler, which transforms AIR (Abstract Intermediate Representation) into Zig source code that performs static analysis at compile time. The generated analyzer catches memory safety issues like use-before-assignment, use-after-free, and stack pointer escapes.

The goal is to bring Rust-level memory safety guarantees to Zig through static analysis of AIR, without changing the language itself.

CLR depends on a forked version of the Zig compiler (included as a submodule in `zig/`) that adds support for routing AIR to external plugins. When invoked with `-ofmt=air -fair-out=<plugin.so>`, the compiler loads the specified shared library and passes generated AIR to it for processing.

## Status

This is an active rewrite of the original Elixir-based proof-of-concept in Zig. The Zig implementation loads as a compiler plugin and analyzes AIR directly.

Currently implemented:
- Undefined value tracking (use before assignment)
- Interprocedural analysis (tracking values across function calls)
- Source location and variable name tracking for error messages

Planned:
- Stack pointer escape detection
- Allocation tracking (use-after-free, double-free, leaks)
- Control flow analysis

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

Output goes to stderr.

## Testing

```sh
# Unit tests
zig build test

# Integration tests (requires BATS)
./run_integration.sh

# Manual test of a single file
./run_one.sh test/cases/undefined/use_before_assign.zig
```

## Project Structure

```
clr/
├── src/
│   ├── clr.zig              # Main CLR plugin entry point
│   ├── codegen.zig          # Generates .air.zig source from AIR instructions
│   └── allocator.zig        # DLL-safe allocator wrapper
├── lib/
│   ├── tag.zig              # AnyTag union and splat dispatch
│   ├── tag/                 # Individual tag handlers
│   ├── analysis/            # Analysis modules (undefined, etc.)
│   └── slots.zig            # Slot struct and apply/call dispatch
├── test/
│   ├── integration/         # BATS integration tests
│   │   ├── test_helper.bash
│   │   └── *.bats
│   └── cases/               # Test input files
├── zig/                     # Zig compiler submodule (instrumented fork)
├── build.zig                # Build configuration
└── build.zig.zon            # Package dependencies
```

## The General Idea

![diagram](./images/diagram.png)

Zig is a famously "unsafe" language. Memory management is done manually, which opens up the possibility of implementation errors. While Zig reduces security issues relative to C by eliminating out-of-bounds array access and null pointer dereferencing in safety-checked code, it is still less safe than Rust, which eliminates use-after-free, double-free, and data races through static analysis.

Inspired by Rust's MIRI project, CLR performs static analysis on Zig's AIR intermediate representation to achieve a higher degree of safety than Zig provides out-of-the-box. Unlike MIRI, which interprets Rust's MIR in a sandboxed pseudo-runtime, CLR transpiles AIR into Zig source code that statically runs an analysis.  Note that CLR's air output zig code could in principle be run at compile time, but by running through a zig intermediate, we produce an easy-to-understand and easy-to-debug logical flow.  An ambitious person might use this general approach to emit a different output target, such as a proof assistant language!

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
