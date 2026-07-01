#!/usr/bin/env bats

load test_helper

assert_stack_escape_trace() {
    local module="$1"
    local detector_function="$2"
    local detector_location="$3"
    local caller_location="$4"
    local origin_message="$5"
    local origin_location="$6"
    local origin_has_caller="$7"
    local detector="stack pointer escape in ${module}.${detector_function}"
    local caller="called from ${module}.main"
    local origin="${origin_message} in ${module}.${detector_function}"

    [[ "$output" == *"$detector"* ]]
    [[ "$output" == *"${module}.zig:${detector_location})"* ]]
    [[ "$output" == *"$caller"* ]]
    [[ "$output" == *"${module}.zig:${caller_location})"* ]]
    [[ "$output" == *"$origin"* ]]
    [[ "$output" == *"${module}.zig:${origin_location})"* ]]

    if [ "$origin_has_caller" = true ]; then
        local after_origin="${output#*"$origin"}"
        [[ "$after_origin" == *"$caller"* ]]
        [[ "$after_origin" == *"${module}.zig:${caller_location})"* ]]
    fi
}

@test "detects stack pointer escape from local variable" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/basic/stack_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in stack_ptr_escape.escaped_ptr" ]]
    [[ "$output" =~ "stack_ptr_escape.zig:4:4)" ]]
    [[ "$output" =~ "called from stack_ptr_escape.main" ]]
    [[ "$output" =~ "stack_ptr_escape.zig:8:25)" ]]
    [[ "$output" =~ "pointer was for local variable 'foo'" ]]
    [[ "$output" =~ "stack_ptr_escape.zig:2:4)" ]]
}

@test "detects stack pointer escape from parameter" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/basic/basic_param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in basic_param_ptr_escape.escaped_param_ptr" ]]
    [[ "$output" =~ "basic_param_ptr_escape.zig:2:4)" ]]
    [[ "$output" =~ "called from basic_param_ptr_escape.main" ]]
    [[ "$output" =~ "basic_param_ptr_escape.zig:6:31)" ]]
    [[ "$output" =~ "pointer was for parameter 'param' created in basic_param_ptr_escape.escaped_param_ptr" ]]
    [[ "$output" =~ "basic_param_ptr_escape.zig:1)" ]]
}

@test "detects indirect stack pointer escape" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/basic/indirect_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in indirect_escape.indirect_escape" ]]
    [[ "$output" =~ "indirect_escape.zig:4:4)" ]]
    [[ "$output" =~ "called from indirect_escape.main" ]]
    [[ "$output" =~ "indirect_escape.zig:8:29)" ]]
    [[ "$output" =~ "pointer was for local variable 'foo'" ]]
    [[ "$output" =~ "indirect_escape.zig:2:4)" ]]
}

@test "no false positive when returning passed-in pointer" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/basic/basic_no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via pointer argument" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/basic/escape_via_ptr_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape in escape_via_ptr_arg.escape_via_arg" ]]
    [[ "$output" =~ "escape_via_ptr_arg.zig:3:4)" ]]
    [[ "$output" =~ "called from escape_via_ptr_arg.main" ]]
    [[ "$output" =~ "escape_via_ptr_arg.zig:8:18)" ]]
    [[ "$output" =~ "pointer was for local variable 'foo'" ]]
    [[ "$output" =~ "escape_via_ptr_arg.zig:2:4)" ]]
}

# =============================================================================
# Union stack pointer tests
# =============================================================================

@test "detects stack pointer escape in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/union/stack_ptr_in_union.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_union.zig" ]]
    assert_stack_escape_trace stack_ptr_in_union getContainer 8:4 12:34 \
        "pointer was for local variable 'x'" 7:4 true
}

@test "detects stack pointer escape via union field" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/union/union_stack_ptr_escaped_via_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_field.zig" ]]
    assert_stack_escape_trace union_stack_ptr_escaped_via_field setContainer 8:4 13:16 \
        "pointer was for local variable 'x'" 7:4 true
}

@test "detects parameter pointer escape in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/union/union_param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
    assert_stack_escape_trace union_param_ptr_escape escaped_param_ptr 7:4 11:39 \
        "pointer was for parameter 'param' created" 6 false
}

@test "no false positive for passed-in pointer in union return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/union/union_no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via union argument" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/union/escape_via_union_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_union_arg.zig" ]]
    assert_stack_escape_trace escape_via_union_arg escape_via_union 8:4 13:20 \
        "pointer was for local variable 'foo'" 7:4 true
}

# =============================================================================
# Struct stack pointer tests
# =============================================================================

@test "detects stack pointer escape in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct/stack_ptr_in_struct.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_struct.zig" ]]
    assert_stack_escape_trace stack_ptr_in_struct getContainer 7:4 11:34 \
        "pointer was for local variable 'x'" 6:4 true
}

@test "detects parameter pointer escape in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct/struct_param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
    assert_stack_escape_trace struct_param_ptr_escape escaped_param_ptr 6:4 10:39 \
        "pointer was for parameter 'param' created" 5 false
}

@test "detects stack pointer escape via struct field" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct/struct_stack_ptr_escaped_via_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_field.zig" ]]
    assert_stack_escape_trace struct_stack_ptr_escaped_via_field setContainer 7:4 12:16 \
        "pointer was for local variable 'x'" 6:4 true
}

@test "no false positive for passed-in pointer in struct return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct/struct_no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via struct argument" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct/escape_via_struct_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_struct_arg.zig" ]]
    assert_stack_escape_trace escape_via_struct_arg escape_via_struct 7:7 12:21 \
        "pointer was for local variable 'foo'" 6:4 true
}

# =============================================================================
# Array (region) stack pointer tests
# =============================================================================

@test "detects stack pointer escape in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/region/stack_ptr_in_array.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_array.zig" ]]
    assert_stack_escape_trace stack_ptr_in_array smuggle_ptr 6:4 10:27 \
        "pointer was for local variable 'local'" 3:4 true
}

@test "detects stack pointer escape via array element" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/region/stack_ptr_escaped_via_element.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_element.zig" ]]
    assert_stack_escape_trace stack_ptr_escaped_via_element escaped_via_element 10:4 14:35 \
        "pointer was for local variable 'x'" 7:4 true
}

@test "detects parameter pointer escape in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/region/region_param_ptr_escape.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "param_ptr_escape.zig" ]]
    assert_stack_escape_trace region_param_ptr_escape escaped_param_ptr 5:4 9:33 \
        "pointer was for parameter 'param' created" 2 false
}

@test "no false positive for passed-in pointer in array return" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/region/region_no_escape.zig"
    [ "$status" -eq 0 ]
}

@test "detects stack pointer escape via array argument" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/region/escape_via_array_arg.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_via_array_arg.zig" ]]
    assert_stack_escape_trace escape_via_array_arg escape_via_arg 11:4 15:30 \
        "pointer was for local variable 'x'" 7:4 true
}

# =============================================================================
# Global stack pointer tests
# =============================================================================

@test "detects stack pointer escape to global variable" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/globals/escape_to_global.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_to_global" ]]
    assert_stack_escape_trace escape_to_global store_stack_ptr 6:4 14:19 \
        "pointer was for local variable 'local'" 5:4 true
}

@test "no false positive for heap pointer in global" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/globals/no_escape_heap.zig"
    [ "$status" -eq 0 ]
}

@test "detects parameter stack pointer escape to global" {
    run compile_and_run "$TEST_CASES/stack_pointer_safety/globals/escape_param_to_global.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "escape_param_to_global" ]]
    assert_stack_escape_trace escape_param_to_global caller 14:19 18:10 \
        "pointer was for local variable 'local'" 13:4 true
}
