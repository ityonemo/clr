#!/usr/bin/env bats

load test_helper

# =============================================================================
# AIR Tag Implementation Tests
# =============================================================================

# These tests verify that AIR tags are properly implemented and don't crash.

@test "ret_addr - return address doesn't crash" {
    run compile_and_run "$TEST_CASES/tags/ret_addr.zig"
    [ "$status" -eq 0 ]
}

@test "aggregate_init - struct initialization doesn't crash" {
    run compile_and_run "$TEST_CASES/tags/aggregate_init.zig"
    [ "$status" -eq 0 ]
}

@test "intcast - integer cast doesn't crash" {
    run compile_and_run "$TEST_CASES/tags/intcast.zig"
    [ "$status" -eq 0 ]
}

@test "slice - slice creation doesn't crash" {
    run compile_and_run "$TEST_CASES/tags/slice.zig"
    [ "$status" -eq 0 ]
}

@test "wrap_errunion_err - error return doesn't crash" {
    run compile_and_run "$TEST_CASES/tags/wrap_errunion_err.zig"
    [ "$status" -eq 0 ]
}
