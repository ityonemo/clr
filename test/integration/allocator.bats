#!/usr/bin/env bats

load test_helper

@test "detects use-after-free" {
    run compile_and_run "$TEST_CASES/allocator/basic/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "detects double-free" {
    run compile_and_run "$TEST_CASES/allocator/basic/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "detects memory leak" {
    run compile_and_run "$TEST_CASES/allocator/basic/memory_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "no false positive for correct allocator usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects freeing stack pointer with allocator" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_stack_pointer.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
}

@test "detects mismatched allocator for create/destroy" {
    run compile_and_run "$TEST_CASES/allocator/basic/mismatched_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
}

@test "no false positive when callee frees allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "detects double-free across caller/callee boundary" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "no false positive when caller frees allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_from_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when caller doesn't free allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_from_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

# Struct pointer field tests

@test "detects use-after-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:17:" ]]
    [[ "$output" =~ "'container.ptr' freed in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' allocated in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:11:" ]]
}

@test "detects double-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:11:" ]]
}

@test "detects memory leak in struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' allocated in leak.main" ]]
    [[ "$output" =~ "leak.zig:11:" ]]
}

@test "no false positive for correct struct pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive when callee frees struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in pass_to_callee_leak.useContainer" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:9:" ]]
    [[ "$output" =~ "'container.ptr' allocated in pass_to_callee_leak.main" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:16:" ]]
}

@test "detects double-free when caller frees after callee freed struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:19:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in pass_to_callee_double_free.freeContainer" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:8:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:15:" ]]
}

@test "detects use-after-free when caller uses struct pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:21:" ]]
    [[ "$output" =~ "'container.ptr' freed in pass_to_callee_use_after_free.freeContainer" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:8:" ]]
    [[ "$output" =~ "'container.ptr' allocated in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:15:" ]]
}

# =============================================================================
# Union pointer field tests
# =============================================================================

@test "no false positive for correct union pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig" ]]
}

@test "detects double-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig" ]]
}

@test "detects memory leak in union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig" ]]
}

@test "no false positive when callee frees union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free when caller frees after callee freed union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free when caller uses union pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}

# =============================================================================
# Union stack pointer tests
# =============================================================================

@test "detects stack pointer escape in union return" {
    run compile_and_run "$TEST_CASES/allocator/stack_pointer/union/stack_ptr_in_union.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_in_union.zig" ]]
}

@test "detects stack pointer escape via union field" {
    run compile_and_run "$TEST_CASES/allocator/stack_pointer/union/stack_ptr_escaped_via_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "stack pointer escape" ]]
    [[ "$output" =~ "stack_ptr_escaped_via_field.zig" ]]
}

@test "no false positive for heap pointer in union" {
    run compile_and_run "$TEST_CASES/allocator/stack_pointer/union/heap_ptr_in_union.zig"
    [ "$status" -eq 0 ]
}
