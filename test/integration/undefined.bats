#!/usr/bin/env bats

load test_helper

@test "detects undefined variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/use_before_assign.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "undefined" ]]
}
