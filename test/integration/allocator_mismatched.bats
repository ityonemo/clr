#!/usr/bin/env bats

load test_helper

# =============================================================================
# Mismatched allocator type tests
# =============================================================================

@test "detects mismatched allocator: comptime page_allocator vs c_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/comptime_page_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "comptime_page_allocator.main" ]]
}

@test "detects mismatched allocator: global GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/global_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "global_gpa.main" ]]
}

@test "detects mismatched allocator: local GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/local_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_gpa.main" ]]
}

@test "detects mismatched allocator: local inline GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/local_inline_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_inline_gpa.main" ]]
}

@test "detects mismatched allocator: local FBA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/local_fba.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_fba.main" ]]
}

@test "detects mismatched allocator: passed allocator vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/mismatched/passed_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "passed_allocator" ]]
}
