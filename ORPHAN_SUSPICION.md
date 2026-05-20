# Orphan Suspicion

The orphan/leak checker is still the main remaining architectural suspicion in CLR's
spatial memory model.

This is not a claim that the current logic is wrong. It is a claim that the logic
still appears to be compensating for earlier model imprecision, and should be
re-validated now that several core refactors have landed.

Recent refactors that change the context:

- branch merge now merges structurally on disagreement instead of picking a winner
- carrier stores (`optional`, `errorunion`, `region`) now copy values instead of
  aliasing source child GIDs
- pointer-producing ops like `ptr_add` and `array_to_slice` now create fresh pointer
  GIDs instead of aliasing source pointer refinements
- `.error_stub` now belongs on payload structure, not wrapper structure
- payload-pointer derivation and derived-pointer ops now panic on impossible
  `.error_stub` sources instead of fabricating state

Because of those changes, the orphan/leak path should be reviewed with fresh tests.

## Current Suspicion

The orphan detection path in `lib/analysis/memory_safety.zig` still contains a lot of
repair logic:

- extra reachability scans through roots and nested structure
- copied-from-branch suppression
- allocator-based matching as a fallback proof of liveness
- branch/orphan checks that appear to defend against stale merge/store behavior

Some of that may still be necessary. But it now looks like historical complexity that
may no longer match the cleaner underlying model.

## Recommended Next Step

Do this test-first.

Add targeted tests for:

- allocation remains reachable after recursive branch merge disagreement
- allocation remains reachable after store through carrier wrappers
- `ptr_add` / `array_to_slice` / slice-derived pointers preserve reachability
  without orphan-specific repair logic
- branch cases that previously depended on allocator-based matching to avoid false
  positive leaks
- real leak cases that should still be reported after branch merge/store cleanup

## Goal

Use those tests to determine which orphan-detection rules are still required and
which are now redundant compensation logic.
