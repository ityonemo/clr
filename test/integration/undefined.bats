#!/usr/bin/env bats

load test_helper

@test "detects undefined variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/basic/use_before_assign.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:3:4)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:2:4)" ]]
}

@test "no error when variable is assigned before use" {
    run compile_and_run "$TEST_CASES/undefined/basic/assigned_before_use.zig"
    [ "$status" -eq 0 ]
}

@test "no error when variable is initialized with value" {
    run compile_and_run "$TEST_CASES/undefined/basic/initialized_with_value.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined variable passed to function" {
    run compile_and_run "$TEST_CASES/undefined/basic/call_with_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in call_with_undefined.main" ]]
    [[ "$output" =~ "call_with_undefined.zig:7:15)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in call_with_undefined.main" ]]
    [[ "$output" =~ "call_with_undefined.zig:6:4)" ]]
}

@test "no false positive when function sets value via pointer" {
    run compile_and_run "$TEST_CASES/undefined/basic/call_sets_via_pointer.zig"
    [ "$status" -eq 0 ]
}

@test "detects inconsistent branch - one branch sets, other doesn't" {
    run compile_and_run "$TEST_CASES/undefined/if/one_branch_sets.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of value that may be undefined in one_branch_sets.main" ]]
    [[ "$output" =~ "one_branch_sets.zig:9:4)" ]]
    [[ "$output" =~ "conditional branch has conflicting status" ]]
    [[ "$output" =~ "one_branch_sets.zig:5:8)" ]]
    [[ "$output" =~ "variable 'x' was set to undefined in one_branch_sets.main" ]]
    [[ "$output" =~ "one_branch_sets.zig:2:4)" ]]
}

@test "detects undefined value returned from branch" {
    run compile_and_run "$TEST_CASES/undefined/if/return_from_both_branches.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
}

@test "no error when both branches set value" {
    run compile_and_run "$TEST_CASES/undefined/if/both_branches_set.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when neither branch sets value" {
    run compile_and_run "$TEST_CASES/undefined/if/neither_branch_sets.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]] || [[ "$output" =~ "may be undefined" ]]
}
