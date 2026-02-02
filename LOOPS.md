# Loop Analysis Logic

This document explains how CLR handles loop analysis for memory safety and other checks.

## Problem Statement

When analyzing loops, a naive approach can produce incorrect results:

1. **Double-free in loops**: When freeing memory inside a loop, each iteration sees the memory as "already freed" from the previous iteration - this should be detected as a double-free.

2. **Conservative cond_br merge inside loops**: A while loop like `while (cond) { body }` generates:
   - `cond_br(cond, body_branch, exit_branch)`
   - The body branch executes and repeats
   - The exit branch uses `br` to exit the loop

   Without special handling, the cond_br merge would combine:
   - Body branch: ptr freed
   - Exit branch: ptr not freed

   Result: "not all branches freed" → ptr appears not freed → **false negative**

## Solution

### 1. Exit Path Capture with `branch_returns` Flag

When a `br` instruction targets a loop's exit block, it:
1. Captures the current state to `frame.br_states` (for later merge)
2. Sets `state.branch_returns = true` to signal "this branch exits"

The `branch_returns` flag tells `cond_br` merge to **exclude** this branch from the local merge. The state was already captured separately.

**Key insight**: Exit paths don't participate in the inner cond_br merge - they're handled at the loop level.

### 2. Loop State Ordering (Index 0 = Null Case)

When collecting states for the final loop merge:
- **Index 0**: Exit states from `br_states` (the "null case" - loop didn't run or exited early)
- **Index 1+**: Iteration states from the fixed-point iteration

This ordering is critical for the merge logic below.

### 3. Memory Safety Merge for Loops

For loop merges (`merge_tag == .loop`), we use different logic than regular branches:

```
Normal branch merge: ALL branches must free → mark as freed
Loop merge: ANY iteration (index 1+) freed → mark as freed
```

We **ignore index 0** (the null case) when determining freed state because:
- The null case represents "loop didn't run"
- If the loop body freed memory, that's the important information
- The exit path's "not freed" state shouldn't dominate

### 4. LoopFrame Structure

```zig
pub const LoopFrame = struct {
    block_idx: usize,                                           // Exit block for br matching
    br_states: AutoHashMap(usize, ArrayList(BrState)),         // Exit states by target block
    deferred_region_gids: AutoHashMap(Gid, void),              // For region tracking (future)
    pre_loop_refinements: ?*Refinements,                       // Snapshot before loop
};
```

- `br_states`: Captures state when `br` exits the loop
- `pre_loop_refinements`: Snapshot for comparing pre-loop vs post-loop state
- `deferred_region_gids`: Reserved for future region-specific suppression

## Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Initial State                            │
│                    ptr.freed = null                         │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Loop Frame Created                        │
│           pre_loop_refinements = clone(state)               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Iteration 1                              │
│  ┌────────────────────┐     ┌────────────────────────────┐ │
│  │   cond_br true     │     │     cond_br false          │ │
│  │   (body branch)    │     │     (exit branch)          │ │
│  │   destroy(ptr)     │     │     br to exit_block       │ │
│  │   ptr.freed = meta │     │     ─────────────────────► │ │
│  └────────────────────┘     │  Captured to br_states     │ │
│           │                 │  branch_returns = true     │ │
│           │                 │  (excluded from merge)     │ │
│           ▼                 └────────────────────────────┘ │
│  cond_br merge (only body branch, exit excluded):          │
│       ptr.freed = meta  ✓                                  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Iteration 2                              │
│  Starts with ptr.freed = meta (from iteration 1)           │
│  destroy(ptr) → double-free detected! ✓                    │
└─────────────────────────────────────────────────────────────┘
```

## Why This Works

1. **Exit path isolation**: The `br` handler marks exit branches as `branch_returns = true`, so cond_br doesn't merge their state. This prevents "not freed in exit path" from masking "freed in body".

2. **Iteration state propagation**: After each iteration, `current_refinements` is updated from the iteration's output. Iteration 2 sees ptr as freed from iteration 1.

3. **Correct merge at loop end**: The loop merge uses the iteration states (index 1+), not the exit states (index 0), to determine the final freed status.

## Files Modified

| File | Change |
|------|--------|
| `lib/Context.zig` | Extended `LoopFrame` with `deferred_region_gids` and `pre_loop_refinements` |
| `lib/Inst.zig` | Updated `loop()` to use new `LoopFrame.init()` and order states correctly |
| `lib/tag.zig` | Added `branch_returns = true` in `Br.apply()` when exiting a loop |
| `lib/analysis/memory_safety.zig` | Updated `merge()` with loop-specific logic ignoring index 0 |

## Test Cases

- `test/cases/allocator/loops/double_free_loop.zig` - Detects double-free (was incorrectly reporting leak)
- `test/cases/allocator/loops/leak_in_loop.zig` - Still correctly detects leaks
