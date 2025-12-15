#!/usr/bin/env bats

load test_helper

@test "detects stack pointer escape from local variable" {
    run compile_and_run "$TEST_CASES/stack_pointer/stack_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
}

@test "detects stack pointer escape from parameter" {
    run compile_and_run "$TEST_CASES/stack_pointer/param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
}

@test "detects indirect stack pointer escape" {
    run compile_and_run "$TEST_CASES/stack_pointer/indirect_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
}

@test "no false positive when returning passed-in pointer" {
    run compile_and_run "$TEST_CASES/stack_pointer/no_escape.zig"
    [ "$status" -eq 0 ]
}
