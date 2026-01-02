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

@test "no false positive for heap pointer in union return" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/no_escape_heap_ptr.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Clobber leak detection tests
# =============================================================================

@test "detects leak when variable is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/variable_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "variable_clobber" ]]
}

@test "detects leak when struct field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/struct_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "struct_field_clobber" ]]
}

@test "detects leak when union variant changes" {
    run compile_and_run "$TEST_CASES/allocator/clobber/union_variant_change.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_variant_change" ]]
}

@test "detects leak when union field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/union_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_field_clobber" ]]
}

@test "detects leak when variable is clobbered in branch" {
    run compile_and_run "$TEST_CASES/allocator/clobber/branch_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "branch_clobber" ]]
}

# =============================================================================
# If/branch memory safety tests
# =============================================================================

@test "detects leak when only one if branch frees" {
    run compile_and_run "$TEST_CASES/allocator/if/free_one_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_branch" ]]
}

@test "no error when both if branches free" {
    run compile_and_run "$TEST_CASES/allocator/if/free_both_branches.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when both if branches free same allocation" {
    run compile_and_run "$TEST_CASES/allocator/if/double_free_branches.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_branches" ]]
}

@test "detects leak when variable is clobbered in if branch" {
    run compile_and_run "$TEST_CASES/allocator/if/clobber_in_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_branch" ]]
}

# =============================================================================
# Switch memory safety tests
# =============================================================================

@test "detects leak when only one switch case frees" {
    run compile_and_run "$TEST_CASES/allocator/switch/free_one_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_case" ]]
}

@test "no error when all switch cases free" {
    run compile_and_run "$TEST_CASES/allocator/switch/free_all_cases.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when multiple switch cases free same allocation" {
    run compile_and_run "$TEST_CASES/allocator/switch/double_free_cases.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_cases" ]]
}

@test "detects leak when variable is clobbered in switch case" {
    run compile_and_run "$TEST_CASES/allocator/switch/clobber_in_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_case" ]]
}

# =============================================================================
# Field pointer free tests
# =============================================================================

@test "detects error when trying to free struct field pointer" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_field_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "free_field_ptr" ]]
}

@test "no error when freeing parent allocation with field pointer in scope" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_parent_ok.zig"
    [ "$status" -eq 0 ]
}
