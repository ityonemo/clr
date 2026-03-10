#!/usr/bin/env bats

load test_helper

@test "detects reading undefined data from allocated pointer" {
    run compile_and_run "$TEST_CASES/misc/allocated_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "undefined" ]]
}

@test "handles nested if statements" {
    run compile_and_run "$TEST_CASES/misc/nested_if.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when early return path doesn't set in-out param" {
    run compile_and_run "$TEST_CASES/misc/late_set_after_branch.zig"
    [ "$status" -ne 0 ]
    # TODO: Flesh out the error message to be more descriptive
    [[ "$output" =~ "conflicting" ]]
}

@test "no false positive for nested block in cond_br branch" {
    # Tests that nested blocks inside cond_br branches have their bodies expanded.
    # Uses std.mem.indexOfSentinel which triggers SIMD code with nested safety blocks.
    run compile_and_run "$TEST_CASES/misc/nested_block_in_cond_br.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.mem.indexOfSentinel SIMD" {
    # Tests that SIMD code in std.mem.indexOfSentinel doesn't trigger false positives.
    # The safety check block inside cond_br branches must have its body properly expanded.
    run compile_and_run "$TEST_CASES/misc/nested_block_optional_simd.zig"
    [ "$status" -eq 0 ]
}

@test "wrap_optional sets memory_safety on optional" {
    # Tests that wrap_optional correctly initializes memory_safety on optionals.
    # Without proper initialization, testValid() would panic.
    run compile_and_run "$TEST_CASES/wrap_optional/basic.zig"
    [ "$status" -eq 0 ]
}
