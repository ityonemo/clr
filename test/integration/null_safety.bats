#!/usr/bin/env bats

load test_helper

@test "detects unwrap of null-initialized optional" {
    run compile_and_run "$TEST_CASES/null_safety/unchecked_unwrap.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "optional unwrap of known null" ]]
    [[ "$output" =~ "unchecked_unwrap.main" ]]
    [[ "$output" =~ "unchecked_unwrap.zig:4:" ]]
}

@test "no error when optional is null-checked before unwrap" {
    run compile_and_run "$TEST_CASES/null_safety/checked_unwrap.zig"
    [ "$status" -eq 0 ]
}

@test "no error when optional is comptime known non-null" {
    run compile_and_run "$TEST_CASES/null_safety/comptime_optional.zig"
    [ "$status" -eq 0 ]
}

@test "no error when using is_null check in else branch" {
    run compile_and_run "$TEST_CASES/null_safety/checked_with_is_null.zig"
    [ "$status" -eq 0 ]
}

@test "no error when optional is assigned non-null value" {
    run compile_and_run "$TEST_CASES/null_safety/assigned_non_null.zig"
    [ "$status" -eq 0 ]
}

@test "detects unwrap of known null value" {
    run compile_and_run "$TEST_CASES/null_safety/assigned_null.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "optional unwrap of known null" ]]
    [[ "$output" =~ "assigned_null.main" ]]
    [[ "$output" =~ "assigned_null.zig:4:" ]]
}
