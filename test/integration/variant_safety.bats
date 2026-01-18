#!/usr/bin/env bats

load test_helper

@test "detects access to inactive union variant" {
    run compile_and_run "$TEST_CASES/variant_safety/inactive_access.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of inactive union variant" ]]
    [[ "$output" =~ "inactive_access.zig" ]]
}

@test "detects pointer access to inactive union variant" {
    run compile_and_run "$TEST_CASES/variant_safety/inactive_ptr_access.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of inactive union variant" ]]
    [[ "$output" =~ "inactive_ptr_access.zig" ]]
}

@test "no error when accessing active union variant" {
    run compile_and_run "$TEST_CASES/variant_safety/active_access.zig"
    [ "$status" -eq 0 ]
}

@test "detects access to old variant after union reassignment" {
    run compile_and_run "$TEST_CASES/variant_safety/wrong_variant_after_change.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of inactive union variant" ]]
    [[ "$output" =~ "wrong_variant_after_change.zig" ]]
}

# Phase 5: tag check filtering
# This test requires recognizing that `if (v.* == .int)` makes .int active in the true branch
@test "no error when union variant is checked before access" {
    run compile_and_run "$TEST_CASES/variant_safety/checked_access.zig"
    [ "$status" -eq 0 ]
}

# Phase 6: Branch merge tests
@test "detects ambiguous variant after branches set different variants" {
    run compile_and_run "$TEST_CASES/variant_safety/branch_different_variants.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of union with ambiguous active variant" ]]
    [[ "$output" =~ "branch_different_variants.zig" ]]
}

@test "no error when both branches set same variant" {
    run compile_and_run "$TEST_CASES/variant_safety/branch_same_variant.zig"
    [ "$status" -eq 0 ]
}

@test "detects ambiguous variant when only one branch changes it" {
    run compile_and_run "$TEST_CASES/variant_safety/branch_one_changes_variant.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of union with ambiguous active variant" ]]
    [[ "$output" =~ "branch_one_changes_variant.zig" ]]
}

# =============================================================================
# Switch Tests
# =============================================================================

@test "detects ambiguous variant after switch cases set different variants" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/different_variants.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of union with ambiguous active variant" ]]
    [[ "$output" =~ "different_variants.zig" ]]
}

@test "no error when all switch cases set same variant" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/same_variant.zig"
    [ "$status" -eq 0 ]
}

@test "detects ambiguous variant when one switch case changes it" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/one_case_changes.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of union with ambiguous active variant" ]]
    [[ "$output" =~ "one_case_changes.zig" ]]
}

@test "no error when switch on union tag accesses correct variant" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/switch_on_union.zig"
    [ "$status" -eq 0 ]
}

@test "no error when switch else branch doesn't access variant" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/switch_else_safe.zig"
    [ "$status" -eq 0 ]
}

@test "detects ambiguous variant access in switch else branch" {
    run compile_and_run "$TEST_CASES/variant_safety/switch/switch_else_unsafe.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of union with ambiguous active variant" ]]
    [[ "$output" =~ "switch_else_unsafe.zig:23" ]]
}

# =============================================================================
# Global union tests
# =============================================================================

@test "detects inactive variant access on global union" {
    run compile_and_run "$TEST_CASES/variant_safety/globals/inactive_access.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "access of inactive union variant" ]]
    [[ "$output" =~ "inactive_access.get_int" ]]
}

@test "no error when accessing active variant of global union" {
    run compile_and_run "$TEST_CASES/variant_safety/globals/active_access.zig"
    [ "$status" -eq 0 ]
}

@test "no error when global union variant is checked before access" {
    run compile_and_run "$TEST_CASES/variant_safety/globals/checked_access.zig"
    [ "$status" -eq 0 ]
}

@test "no error when switch on global union dispatches correctly" {
    run compile_and_run "$TEST_CASES/variant_safety/globals/switch_checked_access.zig"
    [ "$status" -eq 0 ]
}
