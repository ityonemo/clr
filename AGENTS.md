# AGENTS.md

## Project Overview

CLR is a Zig-based static analysis plugin for a forked Zig compiler. The plugin
emits `.air.zig` analyzer programs from Zig AIR, and the runtime library in
`lib/` executes those analyzers to detect memory safety, undefined-value, null,
variant, fieldParentPtr, and file-descriptor issues.

## Important Directories

- `src/`: compiler plugin and AIR-to-Zig code generation.
- `lib/`: runtime analysis library used by generated analyzers.
- `lib/analysis/`: individual safety analyses and their focused tests.
- `test/cases/`: Zig input programs used by integration tests.
- `test/integration/`: BATS integration tests.
- `zig/`: forked Zig compiler submodule. Do not modify unless explicitly asked.
- `vendor/`: vendored validation projects and external inputs.

## Build And Test Commands

- Build plugin: `zig build -Doptimize=ReleaseFast`
- Codegen/plugin tests: `zig build test`
- Runtime library tests: `zig test lib/lib.zig`
- Single test case: `./run_one.sh test/cases/path/to/case.zig`
- Integration test file: `bats test/integration/name.bats`
- Full integration suite: `./run_integration.sh`
- Clean generated analyzer files: `./clear.sh`

Use `ReleaseFast` by default. The vendored Zig compiler and `libclr.so` must be
built with matching optimization modes or plugin loading may crash.

## Development Workflow

For bug fixes and new analysis behavior:

1. Add or identify a failing test before changing implementation code.
2. Prefer a focused unit test in `lib/analysis/*_test.zig` or
   `src/codegen_test.zig` when the bug is localized.
3. Add a minimal Zig case under `test/cases/`.
4. Add BATS coverage under the appropriate `test/integration/*.bats` file.
5. Fix the implementation in `lib/`, `src/`, or both.
6. Run the narrowest relevant tests first.
7. Run the full integration suite once before committing broad changes.
8. Document completed fixes in `FIXES_LOG.md`.

Do not delete, skip, or weaken integration tests to make the suite pass without
explicit permission. Investigate failures instead of assuming they are
pre-existing.

## Debugging Generated Analyzers

`./run_one.sh` generates a root-level `.air.zig` file. After generation, run it
directly for faster iteration:

```sh
zig run --dep clr -Mroot=name.air.zig -Mclr=lib/lib.zig
```

It is acceptable to temporarily instrument generated `.air.zig` files while
debugging. They are ignored by git and can be removed with `./clear.sh`.

For raw AIR around a function, use:

```sh
./dump_air.sh test/cases/path/to/case.zig function.name 80
```

Do not use `-femit-air` or `--verbose-air` directly; use the helper scripts.

## Coding Guidelines

- Match existing Zig style and local helper APIs.
- Keep changes scoped to the affected analysis or codegen path.
- Avoid broad refactors while fixing a specific false positive or false
  negative.
- Commit messages should indicate the commit content contains code written by
  Codex.
- Treat `.air.zig` files, `.zig-cache/`, and `zig-out/` as generated artifacts.
- Do not modify `zig/` unless the task explicitly requires compiler changes.
- Be careful with dirty worktrees; preserve user changes and avoid reverting
  unrelated files.

## Memory Safety Model

Zig-CLR memory analysis is provenance and safety tracking, not Rust-style
single-owner transfer. More than one value or code path may refer to the same
allocation root GID. When any path marks that allocation freed, the ambiguous
safety state collapses around that root and later access/free/leak checks should
use that state. Aliasing will be tracked separately.

- Treat the allocation root GID as the allocation identity.
- Use `allocator_gid` only for allocator mismatch detection.
- Do not model allocation responsibility as exclusive ownership transfer.
- Do not copy a pointer value's `.to` target across `Refinements` tables.
  Cross-table operations may merge/import structure, but pointer values should
  point only at GIDs that already exist in their destination table.
- `ptr_add` and `ptr_sub` both produce derived pointers into the same region.
  Neither operation proves that a pointer has returned to the allocation base.
  Freeing memory through such a derived pointer should remain invalid unless an
  explicit future retag feature, or an internal CLR stdlib override for a known
  Zig stdlib pattern, reestablishes base-allocation provenance. Pointer
  arithmetic must operate on a pointer-to-region; non-pointer or single-item
  pointer inputs should produce a clear analysis error.

## Test Expectations

`zig build test` only covers `src/` tests. When changing runtime analysis logic,
also run `zig test lib/lib.zig`.

Integration tests should assert exit status, error type, function name, source
location, and relevant context messages where applicable.

## Current Implementation Risks

The integration baseline after allocator provenance, call-return, test
reorganization, memory-safety initialization, branch clobber, and copied-argument
reachability work is `346/365` passing (`19` failing), from
`env ZIG_GLOBAL_CACHE_DIR=/tmp/clr-zig-cache ZIG_LOCAL_CACHE_DIR=/tmp/clr-zig-local
./run_integration.sh` on 2026-05-23. Treat remaining failures as real analyzer
work, not compiler-cache failures.

The largest incomplete areas are:

- Interprocedural allocation safety. The remaining allocator failures are
  narrower: global/comptime pointer laundering, allocator mismatch through FBA
  or passed allocators, labeled-switch allocation/free, and stdlib ArrayList
  cleanup. Avoid Rust-style ownership terminology here; the analyzer tracks
  allocation identity, free state, allocator mismatch state, and reachability.
- Current memory-safety failures from the latest full run:
  - `allocator_globals.bats` 98/99: freeing global/comptime memory laundered
    through a function is not reported as `FreeGlobalMemory`.
  - `allocator_mismatched.bats` 105/106: allocator mismatch through local
    `FixedBufferAllocator` and passed allocator is not reported.
  - `labeled_switch.bats` 154: allocation/free across labeled-switch states
    still false-positives.
  - `misc.bats` 209: `std.ArrayList` cleanup still reports a leak in
    `ensureTotalCapacityPrecise`.
- Field pointer provenance. `fieldParentPtr` tracking depends on
  `struct_field_ptr` producing a pointer with field origin metadata. Basic union
  and global union field recovery is covered, but struct/global false positives,
  returned pointers, and merged pointers still expose provenance gaps.
- Current field/stack provenance failures from the latest full run:
  - `fieldparentptr_safety.bats` 144/146: global struct/union field
    `fieldParentPtr` false positives.
  - `stack_pointer.bats` 259: passed-in pointer in struct return is still
    reported as a stack escape.
- FD closure propagation. FD state is scalar-only and copied through some stores
  and aggregate inits, but close state is not reliably propagated to all aliases
  or returned values. Correct open/close examples still report leaks.
- Current FD failures from the latest full run:
  - `fd.bats` 123/126/129/132/133/136 and `undefined.bats` 343. These remain
    punted until the FD aliasing/closure architecture is addressed.
- Stdlib reductions. Pointer/slice conversion, ArrayList, HashMap, and
  indexOfSentinel/SIMD still expose gaps in type/character preservation and
  analysis propagation. Packed struct init, memcpy/memset destination definition,
  string literal equality, branch clobber, and copied struct-argument allocation
  reachability are covered.
- Current stdlib/undefined/null failures from the latest full run:
  - `misc.bats` 196: `std.mem.indexOfSentinel` SIMD path still fails in
    undefined-safety after memory-safety initialization fixes.
  - `misc.bats` 199: bitcast from `[*]u8` to `[*]Struct` does not preserve the
    expected region element type.
  - `misc.bats` 210: `std.HashMap` basic usage currently reaches a
    null-safety unchecked optional unwrap in `HashMapUnmanaged.header`.
- Alignment-cast lowering. `@ptrCast(@alignCast(...))` currently exposes AIR that
  bitcasts a pointer to an address-like scalar, masks low bits, and branches on
  the alignment check. Add an AIR-to-analyzer interceptor that recognizes this
  compiler-generated alignment guard and no-ops the failure branch for current
  analyses; a future `alignment_safety` module can track the actual alignment
  proof separately.
- Pointer arithmetic policy. Codegen marks zero-offset pointer arithmetic so
  full-slice reconstruction such as `ptr[0..len]` can preserve base provenance.
  Nonzero `ptr_add`/`ptr_sub` remains derived provenance; `ptr_sub` does not prove
  recovery of the allocation base. Recovered-base patterns require a future
  retag feature or a narrow internal stdlib override, and pointer stores/loads
  must preserve the pointer value's memory-safety refinement.
- Stack escape false positives. Returning passed-in or heap-backed pointers inside
  structs/unions/globals is still too conservative in several cases.

Problematic patterns to fix before adding more surface area:

- Do not weaken `testValid`. Failures there are signals that handlers wrote
  invalid analysis state.
- Avoid `.scalar` fallback entities for missing or mismatched structure. The
  project guidance says unexpected structure should be copied correctly,
  represented as `.unimplemented`, or crash loudly.
- Avoid `orelse return` for data that should exist. Many handlers silently skip
  missing refinements/globals/variant state; this hides root causes and turns
  failures into later false positives.
- Keep analyte mutation inside `lib/analysis/*.zig`. `lib/tag.zig`,
  `lib/Refinements.zig`, and `lib/Inst.zig` should only build structure and
  dispatch through `splat`, except for tests or explicitly documented generic
  helpers.
- Preserve character across casts and stores. A pointer-like or aggregate value
  should not degrade to scalar just because the apparent Zig type changed.

Good next targets:

1. Continue memory-safety gaps first. Recommended sprint: allocator global
   laundering and allocator mismatch, because they are focused false negatives
   with clear expected error types and should not require the larger stdlib
   remap/ArrayList work.
2. Then handle labeled-switch allocation/free, which likely needs control-flow
   state merge work but is still memory-safety scoped.
3. After that, decide between ArrayList remap/reallocation cleanup and the
   fieldParentPtr/stack-pointer provenance false positives.
4. Leave FD aliasing until later unless a narrower FD bug blocks another fix.
