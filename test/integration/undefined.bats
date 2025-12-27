#!/usr/bin/env bats

load test_helper

@test "detects undefined variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/basic/use_before_assign.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:3:4)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in use_before_assign.main" ]]
    [[ "$output" =~ "use_before_assign.zig:2:4)" ]]
}

@test "no error when variable is assigned before use" {
    run compile_and_run "$TEST_CASES/undefined/basic/assigned_before_use.zig"
    [ "$status" -eq 0 ]
}

@test "no error when variable is initialized with value" {
    run compile_and_run "$TEST_CASES/undefined/basic/initialized_with_value.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined variable passed to function" {
    run compile_and_run "$TEST_CASES/undefined/basic/call_with_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in call_with_undefined.main" ]]
    [[ "$output" =~ "call_with_undefined.zig:7:15)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in call_with_undefined.main" ]]
    [[ "$output" =~ "call_with_undefined.zig:6:4)" ]]
}

@test "no false positive when function sets value via pointer" {
    run compile_and_run "$TEST_CASES/undefined/basic/call_sets_via_pointer.zig"
    [ "$status" -eq 0 ]
}

@test "detects inconsistent branch - one branch sets, other doesn't" {
    run compile_and_run "$TEST_CASES/undefined/if/one_branch_sets.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of value that may be undefined in one_branch_sets.main" ]]
    [[ "$output" =~ "one_branch_sets.zig:9:4)" ]]
    [[ "$output" =~ "conditional branch has conflicting status" ]]
    [[ "$output" =~ "one_branch_sets.zig:5:8)" ]]
    [[ "$output" =~ "variable 'x' was set to undefined in one_branch_sets.main" ]]
    [[ "$output" =~ "one_branch_sets.zig:2:4)" ]]
}

@test "detects undefined value returned from branch" {
    run compile_and_run "$TEST_CASES/undefined/if/return_from_both_branches.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in return_from_both_branches.returns_from_both_branches" ]]
    [[ "$output" =~ "return_from_both_branches.zig:12:8)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in return_from_both_branches.returns_from_both_branches" ]]
    [[ "$output" =~ "return_from_both_branches.zig:7:4)" ]]
}

@test "no error when both branches set value" {
    run compile_and_run "$TEST_CASES/undefined/if/both_branches_set.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when both branches set to undefined" {
    run compile_and_run "$TEST_CASES/undefined/if/both_branches_unset.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in both_branches_unset.main" ]]
    [[ "$output" =~ "both_branches_unset.zig:11:4)" ]]
    [[ "$output" =~ "undefined value assigned to 'x' in both_branches_unset.main" ]]
    [[ "$output" =~ "both_branches_unset.zig:6:8)" ]]
}

@test "no error when optional set to value or null in branches" {
    run compile_and_run "$TEST_CASES/undefined/if/optional_set_or_null.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Struct Tests
# =============================================================================

@test "detects undefined struct field access" {
    run compile_and_run "$TEST_CASES/undefined/structs/undefined_field_access.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in undefined_field_access.main" ]]
    [[ "$output" =~ "undefined_field_access.zig:10:" ]]
    # When entire struct is set to undefined, the name is the struct variable name
    [[ "$output" =~ "undefined value assigned to 'p' in undefined_field_access.main" ]]
    [[ "$output" =~ "undefined_field_access.zig:7:" ]]
}

@test "no error when undefined struct field is not accessed" {
    run compile_and_run "$TEST_CASES/undefined/structs/undefined_field_not_accessed.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined struct field when struct is passed" {
    run compile_and_run "$TEST_CASES/undefined/structs/undefined_struct_passed.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "undefined_struct_passed.zig" ]]
}

@test "detects undefined field accessed through passed pointer" {
    run compile_and_run "$TEST_CASES/undefined/structs/undefined_field_accessed_through_pass.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "undefined_field_accessed_through_pass.zig" ]]
}

@test "no error when struct is fully defined via passed pointer" {
    run compile_and_run "$TEST_CASES/undefined/structs/struct_defined_via_pass.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined field when callee misses setting it" {
    run compile_and_run "$TEST_CASES/undefined/structs/struct_field_missed_when_passed.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in struct_field_missed_when_passed.main" ]]
    [[ "$output" =~ "struct_field_missed_when_passed.zig:15:" ]]
}

@test "no error when field-level undefined is set before use" {
    run compile_and_run "$TEST_CASES/undefined/structs/field_level_undefined.zig"
    [ "$status" -eq 0 ]
}

@test "no error when partial init with default sets undefined field before use" {
    run compile_and_run "$TEST_CASES/undefined/structs/partial_default_init.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined field x when default not applied with = undefined" {
    run compile_and_run "$TEST_CASES/undefined/structs/full_undefined_with_default.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in full_undefined_with_default.main" ]]
    [[ "$output" =~ "full_undefined_with_default.zig:9:" ]]
}

@test "no error when accessing defined field from partial init const" {
    run compile_and_run "$TEST_CASES/undefined/structs/partial_init_const_defined_field.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined field from partial init const" {
    run compile_and_run "$TEST_CASES/undefined/structs/partial_init_const_undefined_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "partial_init_const_undefined_field.zig" ]]
}
