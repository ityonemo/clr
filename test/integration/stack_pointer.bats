#!/usr/bin/env bats

load test_helper

@test "detects stack pointer escape from local variable" {
    run compile_and_run "$TEST_CASES/stack_pointer/basic/stack_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in stack_ptr_escape.escaped_ptr" ]]
    [[ "$output" =~ "stack_ptr_escape.zig:4:4)" ]]
    [[ "$output" =~ "pointer was for local variable 'foo'" ]]
    [[ "$output" =~ "stack_ptr_escape.zig:2:4)" ]]
}

@test "detects stack pointer escape from parameter" {
    run compile_and_run "$TEST_CASES/stack_pointer/basic/param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in param_ptr_escape.escaped_param_ptr" ]]
    [[ "$output" =~ "param_ptr_escape.zig:2:4)" ]]
    [[ "$output" =~ "pointer was for parameter 'param' created in param_ptr_escape.escaped_param_ptr" ]]
    [[ "$output" =~ "param_ptr_escape.zig:1)" ]]
}

@test "detects indirect stack pointer escape" {
    run compile_and_run "$TEST_CASES/stack_pointer/basic/indirect_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in indirect_escape.indirect_escape" ]]
    [[ "$output" =~ "indirect_escape.zig:4:4)" ]]
    [[ "$output" =~ "pointer was for local variable 'foo'" ]]
    [[ "$output" =~ "indirect_escape.zig:2:4)" ]]
}

@test "no false positive when returning passed-in pointer" {
    run compile_and_run "$TEST_CASES/stack_pointer/basic/no_escape.zig"
    [ "$status" -eq 0 ]
}
