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
