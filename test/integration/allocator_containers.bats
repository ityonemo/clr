#!/usr/bin/env bats

load test_helper

# =============================================================================
# Struct pointer field tests
# =============================================================================

@test "detects use-after-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:17:" ]]
    [[ "$output" =~ "'container.ptr' freed in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' allocated in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:11:" ]]
}

@test "detects double-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:11:" ]]
}

@test "detects memory leak in struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' allocated in leak.main" ]]
    [[ "$output" =~ "leak.zig:11:" ]]
}

@test "no false positive for correct struct pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive when callee frees struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in pass_to_callee_leak.main" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:20:" ]]
    [[ "$output" =~ "'container.ptr' allocated in pass_to_callee_leak.main" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:16:" ]]
}

@test "detects double-free when caller frees after callee freed struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:19:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in pass_to_callee_double_free.freeContainer" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:8:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:15:" ]]
}

@test "detects use-after-free when caller uses struct pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/pass_to_callee_use_after_free.zig"
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
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig" ]]
}

@test "detects double-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig" ]]
}

@test "detects memory leak in union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig" ]]
}

@test "no false positive when callee frees union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free when caller frees after callee freed union pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free when caller uses union pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}

@test "no false positive for heap pointer in union return" {
    run compile_and_run "$TEST_CASES/allocator_safety/union_pointer_field/no_escape_heap_ptr.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Array pointer field tests
# =============================================================================

@test "no false positive for correct array pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free through array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig" ]]
}

@test "detects double-free through array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig" ]]
}

@test "detects memory leak in array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig" ]]
}

@test "no false positive when callee frees array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free when caller frees after callee freed array pointer field" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free when caller uses array pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator_safety/array_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}
