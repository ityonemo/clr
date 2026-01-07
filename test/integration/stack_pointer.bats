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

@test "detects stack pointer escape via pointer argument" {
    run compile_and_run "$TEST_CASES/stack_pointer/basic/escape_via_ptr_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_ptr_arg.zig" ]]
}

# =============================================================================
# Union stack pointer tests
# =============================================================================

@test "detects stack pointer escape in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer/union/stack_ptr_in_union.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_union.zig" ]]
}

@test "detects stack pointer escape via union field" {
    run compile_and_run "$TEST_CASES/stack_pointer/union/stack_ptr_escaped_via_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_field.zig" ]]
}

@test "detects parameter pointer escape in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer/union/param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
}

@test "no false positive for passed-in pointer in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer/union/no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via union argument" {
    run compile_and_run "$TEST_CASES/stack_pointer/union/escape_via_union_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_union_arg.zig" ]]
}

# =============================================================================
# Struct stack pointer tests
# =============================================================================

@test "detects stack pointer escape in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer/struct/stack_ptr_in_struct.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_struct.zig" ]]
}

@test "detects parameter pointer escape in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer/struct/param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
}

@test "detects stack pointer escape via struct field" {
    run compile_and_run "$TEST_CASES/stack_pointer/struct/stack_ptr_escaped_via_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_field.zig" ]]
}

@test "no false positive for passed-in pointer in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer/struct/no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via struct argument" {
    run compile_and_run "$TEST_CASES/stack_pointer/struct/escape_via_struct_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_struct_arg.zig" ]]
}

# =============================================================================
# Array (region) stack pointer tests
# =============================================================================

@test "detects stack pointer escape in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer/region/stack_ptr_in_array.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_array.zig" ]]
}

@test "detects stack pointer escape via array element" {
    run compile_and_run "$TEST_CASES/stack_pointer/region/stack_ptr_escaped_via_element.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_element.zig" ]]
}

@test "detects parameter pointer escape in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer/region/param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
}

@test "no false positive for passed-in pointer in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer/region/no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via array argument" {
    run compile_and_run "$TEST_CASES/stack_pointer/region/escape_via_array_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_array_arg.zig" ]]
}
