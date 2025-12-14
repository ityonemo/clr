#!/usr/bin/env bats

load test_helper

@test "detects undefined variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/use_before_assign.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:3:4)" ]]
}

@test "no error when variable is assigned before use" {
    run compile_and_run "$TEST_CASES/undefined/assigned_before_use.zig"
    [ "$status" -eq 0 ]
}

@test "no error when variable is initialized with value" {
    run compile_and_run "$TEST_CASES/undefined/initialized_with_value.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined variable passed to function" {
    run compile_and_run "$TEST_CASES/undefined/call_with_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
}

@test "no false positive when function sets value via pointer" {
    run compile_and_run "$TEST_CASES/undefined/call_sets_via_pointer.zig"
    [ "$status" -eq 0 ]
}
