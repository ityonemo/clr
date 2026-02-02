# Better Error Analysis

## Problem

When allocations are made **inside loops** through functions that return error unions (like `allocator.create()`), the current analysis creates allocation metadata **before** we know if the allocation succeeded or failed. This causes false-positive memory leak reports when early returns capture error path state.

**Note**: Straight-line code with `try`/`catch` works correctly. The issue is specifically the interaction between **loops** and **error handling early returns**.

### Example

```zig
pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;  // Error path triggers false leak
        ptr.* = i;
        allocator.destroy(ptr);
    }
    return 0;
}
```

### What Happens

1. `alloc_create` creates: `errorunion → pointer → pointee`
   - Sets allocation metadata on **all three** entities immediately

2. `is_non_err` checks if allocation succeeded (produces bool)

3. `cond_br` branches:
   - **TRUE branch**: `unwrap_errunion_payload` extracts pointer, code continues, `destroy` frees it
   - **FALSE branch**: `ret_safe` returns early (the `catch return 1`)

4. The FALSE branch captures an early_return state where:
   - Allocation metadata exists on the pointer entity (GID 21)
   - But the allocation **didn't actually happen** (we're on the error path)
   - `alloc_destroy` never ran

5. At function end, `mergeEarlyReturns` checks for orphaned allocations
   - Finds GID 21 with `allocated, freed=false`
   - Reports false-positive memory leak

### Root Cause

`is_non_err` is currently a `Simple` tag that just produces a boolean scalar. It doesn't track which errorunion it's checking. So when `cond_br` takes the FALSE branch (error path), we can't trace back to clear the allocation metadata from that errorunion's payload.

## Proposed Solution

### Option 1: Extend `is_non_err` to Track Source (Recommended)

1. **Update codegen** to pass the source errorunion operand to `is_non_err`:
   ```zig
   // In src/codegen.zig, add is_non_err to payload() switch:
   .is_non_err => payloadUnOp(info, datum),  // Same pattern as is_non_null
   ```

2. **Change `is_non_err` tag type** from `Simple` to a struct with source:
   ```zig
   // In lib/tag.zig:
   pub const IsNonErr = struct {
       src: Src,  // The errorunion being checked

       pub fn apply(self: @This(), state: State, index: usize) !void {
           // Create bool scalar for result
           _ = try Inst.clobberInst(state.refinements, state.results, index, .{ .scalar = .{} });
           try splat(.is_non_err, state, index, self);
       }
   };
   ```

3. **Add memory_safety handler for `cond_br`** that checks if condition came from `is_non_err`:
   ```zig
   // In lib/analysis/memory_safety.zig:
   pub fn cond_br(state: State, index: usize, params: tag.CondBr) !void {
       // Only handle FALSE branch (error path)
       if (params.branch) return;

       // Check if condition came from is_non_err
       const cond_idx = params.condition_idx orelse return;
       const cond_tag = state.results[cond_idx].inst_tag orelse return;
       if (cond_tag != .is_non_err) return;

       // Get the errorunion that was checked
       // (is_non_err now tracks its source)
       const is_non_err_params = ...; // Need to retrieve from somewhere
       const eu_gid = resolveSource(state, is_non_err_params.src);
       const eu_ref = state.refinements.at(eu_gid);

       // Clear allocation metadata from errorunion's payload
       const payload_gid = eu_ref.errorunion.to;
       clearAllocationMetadata(state.refinements, payload_gid);
   }
   ```

### Option 2: Delay Allocation Metadata Until Unwrap

Instead of setting allocation metadata in `alloc_create`, only set it when `unwrap_errunion_payload` successfully extracts the pointer.

**Pros**: Simpler conceptually - metadata only exists when allocation succeeded
**Cons**: Need to mark the errorunion somehow so `unwrap_errunion_payload` knows to set metadata

### Option 3: Heuristic in Orphan Check

Skip leak checks for pointers that are inside orphaned errorunions.

**Pros**: No codegen changes needed
**Cons**: Heuristic might miss real leaks or have other edge cases

## Implementation Steps

1. Add `is_non_err` to `payloadUnOp` in `src/codegen.zig` (single line change)
2. Create `IsNonErr` struct in `lib/tag.zig` with `src: Src` field
3. Add `cond_br` handler in `lib/analysis/memory_safety.zig`
4. Add helper to clear allocation metadata recursively
5. Add tests for error path scenarios

## Test Cases Needed

- `catch return` with allocation (current failing case)
- `try` with allocation in function returning error
- Nested error unions
- Error union stored in variable before check
- Multiple allocations with single error check

## Error Union Tags Overview

### Current Tag Support

| Tag | Implementation | Purpose |
|-----|----------------|---------|
| `alloc_create` | ✅ Full | Creates `errorunion → pointer → pointee`, sets allocation metadata |
| `alloc_alloc` | ✅ Full | Creates `errorunion → pointer → region → element` |
| `unwrap_errunion_payload` | ✅ Full | Extracts payload GID from errorunion (success path) |
| `@"try"` | ✅ Full | Same as `unwrap_errunion_payload` |
| `wrap_errunion_payload` | ✅ Full | Wraps value in errorunion |
| `errunion_payload_ptr_set` | ✅ Full | Gets `*T` from `*(E!T)` |
| `is_non_err` | ⚠️ Simple | Produces bool, **doesn't track source errorunion** |
| `unwrap_errunion_err` | ⚠️ Simple | Produces error scalar, **no source tracking** |
| `wrap_errunion_err` | ❌ Unimplemented | |

### `try` vs `catch` Control Flow

Both `try` and `catch` generate similar AIR patterns with different error handling:

**`try` expression:**
```zig
const ptr = try allocator.create(u8);
```
AIR:
1. `alloc_create` → errorunion (inst 17)
2. `is_non_err` → bool (inst 18)
3. `cond_br`:
   - TRUE: `unwrap_errunion_payload` → continue with pointer
   - FALSE: `unwrap_errunion_err` → extract error, then `ret_safe` (propagate error)

**`catch return` expression:**
```zig
const ptr = allocator.create(u8) catch return 1;
```
AIR:
1. `alloc_create` → errorunion (inst 17)
2. `is_non_err` → bool (inst 18)
3. `cond_br`:
   - TRUE: `unwrap_errunion_payload` → continue with pointer
   - FALSE: `ret_safe` with literal value (early return)

**`catch` with handler:**
```zig
const ptr = allocator.create(u8) catch |err| {
    log.err("failed: {}", .{err});
    return 1;
};
```
AIR:
1. `alloc_create` → errorunion (inst 17)
2. `is_non_err` → bool (inst 18)
3. `cond_br`:
   - TRUE: `unwrap_errunion_payload` → continue with pointer
   - FALSE: `unwrap_errunion_err` → get error value, execute handler, `ret_safe`

### The `unwrap_errunion_err` Merge Problem

When `unwrap_errunion_err` extracts the error value from an errorunion, it means we're on the error path. At this point:
1. The errorunion's payload (pointer to allocation) is **not active**
2. But the allocation metadata still exists on that payload entity
3. If this error path returns or continues, the "inactive" allocation can trigger false positives

**Key insight**: `unwrap_errunion_err` is effectively a signal that we've entered an error handling path where the errorunion's payload should be considered "dead" for analysis purposes.

### Why Straight-Line Code Works (By Accident)

In straight-line code (no loops):
```zig
pub fn example() !void {
    const ptr = try allocator.create(u8);  // GID 10
    allocator.destroy(ptr);
}
```

1. Allocation created at GID 10
2. Error path creates early_return with GID 10's state
3. No loop merge happens
4. At function end, `base_len` = current refinements length (includes GID 10)
5. Orphan check: GID 10 < base_len → **not orphaned**, no leak check runs

**IMPORTANT**: This passes **by accident**, not because we correctly understand error semantics.

The allocation metadata **still exists** in the early_return state from the error path. We just don't check it because the entity isn't "orphaned" (its GID is below base_len). The analysis doesn't actually know that the error path's allocation didn't happen at runtime.

This is fragile and semantically incorrect. The correct behavior would be:
1. **Know** we're on an error path (via `is_non_err` + `cond_br` FALSE branch)
2. **Clear** the allocation metadata from the errorunion's payload
3. **Then** the early_return state correctly represents "no allocation happened"

Currently we rely on a side effect of orphan detection mechanics, not on correct semantic understanding of error paths.

### Why Loop-Internal Allocations Fail

In loop code:
```zig
while (i < 3) : (i += 1) {
    const ptr = allocator.create(u8) catch return 1;  // GID 20 (inside loop)
    allocator.destroy(ptr);
}
```

1. Loop body creates allocation at GID 20 (above pre-loop base of 11)
2. Error path creates early_return capturing GID 20's state
3. Loop merge shrinks refinements (GID 20+ discarded, base becomes 12)
4. Early_return still references GID 20 with allocation metadata
5. At function end, `mergeEarlyReturns` uses `base_len` = 14 (post-loop)
6. Orphan check: GID 20 >= 14 in early_return → **orphaned with unfreed allocation** → false leak

### Early Returns and Merge Points

Error paths typically end with some form of return:
- `ret_safe` - returns a value (including propagated errors)
- `ret_load` - returns by loading from pointer
- `br` to exit block - breaks out of containing scope

These create **early_return states** that are collected and merged at function end via `mergeEarlyReturns()`. The merge:
1. Collects all early_return states
2. Checks for orphaned entities in each state
3. Reports leaks for unfreed allocations

The problem: early_return states from error paths contain allocation metadata from `alloc_create`, even though those allocations never succeeded.

### Solution Requirements

The fix needs to handle the point where we **know** we're on an error path:

1. **At `is_non_err` check**: Track which errorunion is being checked
2. **At `cond_br` FALSE branch**: Clear allocation metadata from the checked errorunion's payload
3. **OR at `unwrap_errunion_err`**: Clear allocation metadata from the source errorunion's payload

This ensures that when early_return states are captured on error paths, they don't contain allocation metadata for allocations that never happened.

## Related Files

- `src/codegen.zig` - Add `is_non_err` payload generation
- `lib/tag.zig` - Change `is_non_err` from `Simple` to `IsNonErr`
- `lib/analysis/memory_safety.zig` - Add `cond_br` handler for error path cleanup

## Skipped Tests

The following tests are skipped pending this fix:
- `test/integration/loops.bats`: "no error when allocating and freeing in same iteration"
- `test/integration/loops.bats`: "no error with correct nested loop cleanup"
