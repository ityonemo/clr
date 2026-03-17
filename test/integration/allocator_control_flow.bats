#!/usr/bin/env bats

load test_helper

# =============================================================================
# Clobber leak detection tests
# =============================================================================

@test "detects leak when variable is clobbered" {
    run compile_and_run "$TEST_CASES/allocator_safety/clobber/variable_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "variable_clobber" ]]
}

@test "detects leak when struct field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator_safety/clobber/struct_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "struct_field_clobber" ]]
}

@test "detects leak when union variant changes" {
    run compile_and_run "$TEST_CASES/allocator_safety/clobber/union_variant_change.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_variant_change" ]]
}

@test "detects leak when union field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator_safety/clobber/union_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_field_clobber" ]]
}

@test "detects leak when variable is clobbered in branch" {
    run compile_and_run "$TEST_CASES/allocator_safety/clobber/branch_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "branch_clobber" ]]
}

# =============================================================================
# If/branch memory safety tests
# =============================================================================

@test "detects leak when only one if branch frees" {
    run compile_and_run "$TEST_CASES/allocator_safety/if/free_one_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_branch" ]]
}

@test "no error when both if branches free" {
    run compile_and_run "$TEST_CASES/allocator_safety/if/free_both_branches.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when both if branches free same allocation" {
    run compile_and_run "$TEST_CASES/allocator_safety/if/double_free_branches.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_branches" ]]
}

@test "detects leak when variable is clobbered in if branch" {
    run compile_and_run "$TEST_CASES/allocator_safety/if/clobber_in_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_branch" ]]
}

@test "no false positive when if branch frees and returns" {
    run compile_and_run "$TEST_CASES/allocator_safety/if/free_and_return.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Switch memory safety tests
# =============================================================================

@test "detects leak when only one switch case frees" {
    run compile_and_run "$TEST_CASES/allocator_safety/switch/free_one_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_case" ]]
}

@test "no error when all switch cases free" {
    run compile_and_run "$TEST_CASES/allocator_safety/switch/free_all_cases.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when multiple switch cases free same allocation" {
    run compile_and_run "$TEST_CASES/allocator_safety/switch/double_free_cases.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_cases" ]]
}

@test "detects leak when variable is clobbered in switch case" {
    run compile_and_run "$TEST_CASES/allocator_safety/switch/clobber_in_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_case" ]]
}

@test "no false positive when switch case frees and returns" {
    run compile_and_run "$TEST_CASES/allocator_safety/switch/free_and_return.zig"
    [ "$status" -eq 0 ]
}
