# TDD Workflow for Vendor Wrapper Issues

**IMPORTANT**: Every fix MUST follow this cycle:

## Step 1: Identify the Error
Run the vendor wrapper and capture the error:
```bash
./run_one.sh vendor/validate/clr_wrapper.zig 2>&1 | head -30
```

## Step 2: Understand the Error
Analyze the error to understand its root cause:
- Read the stack trace and identify the failing code path
- If you are highly confident about the cause, skip to Step 3
- If uncertain, instrument the generated `.air.zig` file to gather more information:
  - Add debug prints to trace refinement state at key points
  - Run directly: `zig run --dep clr -Mroot=test.air.zig -Mclr=lib/lib.zig`
- If the error is in stdlib code, follow the **Stdlib Reduction Cycle** below

### Stdlib Reduction Cycle (for errors in stdlib)

When the error originates in stdlib code, follow this 3-phase reduction:

**Phase 1: Reduce to simplest stdlib-only example**
- Create a minimal `.zig` file in `test/cases/` that uses ONLY stdlib functions
- Strip away all vendor code, keep just the stdlib call chain that triggers the error
- Example: If error is in `hash_map.getIndex`, create a test that just uses HashMap

**Phase 2: Vendor stdlib functions and reduce further**
- Copy the relevant stdlib functions into the test file
- Replace stdlib calls with local versions
- Reduce to basic Zig control flow + the intercepted stdlib function
- This isolates the exact code pattern causing the false positive

**Phase 3: Instrument the .air.zig**
- Run `./run_one.sh` on the reduced test to generate the `.air.zig` file
- Add debug prints to the `.air.zig` to trace refinement state
- Run directly: `zig run --dep clr -Mroot=test.air.zig -Mclr=lib/lib.zig`
- Identify exactly where the analysis goes wrong

Once you understand the root cause, proceed to Step 3.

## Step 3: Write a Unit Test
Write a unit test in `lib/analysis/*_test.zig` or `src/codegen_test.zig` that exercises the specific component:
- Test the tag handler, codegen function, or analysis module directly
- Verify the test fails before implementing the fix

## Step 4: Write an Integration Test Case
Create a small `.zig` file in `test/cases/` that triggers the SAME error:
- Extract the minimal pattern from the stdlib/vendor code
- The test should be standalone (no external dependencies if possible)
- Name it descriptively (e.g., `slice_elem_val_on_slice.zig`)

## Step 5: Add Integration Test to BATS
Add the test to `test/integration/misc.bats` (or appropriate file):
```bash
@test "description of what should work" {
    run compile_and_run "$TEST_CASES/path/to/test.zig"
    [ "$status" -eq 0 ]  # or appropriate assertion
}
```

## Step 6: Verify Tests Fail
```bash
zig test lib/lib.zig  # Unit tests
bats test/integration/misc.bats -f "test name pattern"  # Integration test
```
Tests MUST fail before implementing the fix.

## Step 7: Implement the Fix
Now fix the issue in `lib/tag.zig`, `src/codegen.zig`, `lib/analysis/*.zig`, etc.

## Step 8: Verify Tests Pass
```bash
zig test lib/lib.zig  # Unit tests
bats test/integration/misc.bats -f "test name pattern"  # Integration test
```

## Step 9: Run Full Integration Tests (Before Commit)
```bash
./run_integration.sh 2>&1 | tee /tmp/integration_results.txt; echo "Exit code: $?"
```

## Step 10: Document the Fix
Add an entry to `FIXES_LOG.md` with:
- Date
- Symptom (the error message/behavior observed)
- Root Cause (what was wrong)
- When This Happens (what triggers the bug)
- The Fix (what was changed)
- Test references (unit test and integration test)

## Step 11: Commit
Only after all tests pass.

## Step 12: Go Back to Step 1
Return to Step 1 and repeat the cycle for the next error.

---

## Verification Commands

```bash
# Run all unit tests
zig test lib/lib.zig

# Run codegen tests
zig build test

# Run full integration suite
./run_integration.sh

# Check for analyte access outside analysis modules
grep -r "\.analyte\." lib/ --include="*.zig" | grep -v "lib/analysis/" | grep -v "_test.zig"
```
