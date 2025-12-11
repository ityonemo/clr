#!/usr/bin/env bats

load test_helper

@test "detects undefined variable used before assignment" {
    run compile_air "$TEST_CASES/undefined/use_before_assign.zig" "$TEST_TEMP/use_before_assign.air"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "undefined" ]]
}
