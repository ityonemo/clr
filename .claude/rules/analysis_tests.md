I---
paths:
  - "lib/analysis/*_test.zig"
---

# Analysis Module Tests

## Purpose

Unit tests for analysis modules. These test the analysis logic directly without going through the full compilation pipeline.

## Test Pattern

Tests typically:
1. Create a `Refinements` table
2. Create `Inst` results array
3. Set up refinement structure manually
4. Call analysis handlers directly
5. Verify the resulting state or errors

## Running Tests

```sh
zig test lib/lib.zig
```

Note: `zig build test` only runs `src/` tests. Always run both when modifying analysis logic.

## When to Write Unit Tests

- Before fixing a bug (reproduce the failure first)
- When adding a new handler to an analysis module
- To test edge cases that are hard to trigger via integration tests

Unit tests run in seconds; use them for fast iteration before running integration tests.
