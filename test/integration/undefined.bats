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
# Switch Tests
# =============================================================================

@test "detects undefined when one switch case sets, others don't" {
    run compile_and_run "$TEST_CASES/undefined/switch/one_case_sets.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of value that may be undefined in one_case_sets.main" ]]
    [[ "$output" =~ "one_case_sets.zig:" ]]
}

@test "no error when all switch cases set value" {
    run compile_and_run "$TEST_CASES/undefined/switch/all_cases_set.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when all switch cases set to undefined" {
    run compile_and_run "$TEST_CASES/undefined/switch/all_cases_unset.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in all_cases_unset.main" ]]
    [[ "$output" =~ "all_cases_unset.zig:" ]]
}

@test "detects undefined value returned from switch case" {
    run compile_and_run "$TEST_CASES/undefined/switch/return_from_cases.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "return_from_cases.zig:" ]]
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

@test "detects undefined value when dereferencing pointer to undefined field" {
    run compile_and_run "$TEST_CASES/undefined/structs/field_ptr_to_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in field_ptr_to_undefined.main" ]]
    [[ "$output" =~ "field_ptr_to_undefined.zig" ]]
}

@test "no error when dereferencing pointer to defined field" {
    run compile_and_run "$TEST_CASES/undefined/structs/field_ptr_to_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no error when field is defined through field pointer" {
    run compile_and_run "$TEST_CASES/undefined/structs/field_ptr_defines_field.zig"
    [ "$status" -eq 0 ]
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

# =============================================================================
# Union Tests - Tagged
# =============================================================================

@test "detects undefined tagged union field access" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_undefined_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in tagged_undefined_field.main" ]]
    [[ "$output" =~ "tagged_undefined_field.zig" ]]
}

@test "no error when tagged union field is defined" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_defined_field.zig"
    [ "$status" -eq 0 ]
}

@test "no error when undefined tagged union field is not accessed" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_field_not_accessed.zig"
    [ "$status" -eq 0 ]
}

@test "no error when tagged union is set via pointer" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_set_via_pointer.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined tagged union passed to function" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_passed_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "tagged_passed_undefined.zig" ]]
}

@test "detects undefined tagged union field accessed through pass" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_accessed_through_pass.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in" ]]
    [[ "$output" =~ "tagged_accessed_through_pass.zig" ]]
}

@test "no error when tagged union field is defined via pass" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_defined_via_pass.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when callee misses setting tagged union field" {
    run compile_and_run "$TEST_CASES/undefined/unions/tagged_field_missed_when_passed.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in tagged_field_missed_when_passed.main" ]]
    [[ "$output" =~ "tagged_field_missed_when_passed.zig" ]]
}

# =============================================================================
# Union Tests - Untagged
# =============================================================================

@test "detects undefined untagged union field access" {
    run compile_and_run "$TEST_CASES/undefined/unions/untagged_undefined_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in untagged_undefined_field.main" ]]
    [[ "$output" =~ "untagged_undefined_field.zig" ]]
}

@test "no error when untagged union field is defined" {
    run compile_and_run "$TEST_CASES/undefined/unions/untagged_defined_field.zig"
    [ "$status" -eq 0 ]
}

@test "no error when untagged union is set via pointer" {
    run compile_and_run "$TEST_CASES/undefined/unions/untagged_set_via_pointer.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Region Tests (Arrays)
# =============================================================================

@test "detects undefined array element access" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_undefined_element.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_undefined_element.main" ]]
    [[ "$output" =~ "array_undefined_element.zig:4:" ]]
    [[ "$output" =~ "undefined value assigned in array_undefined_element.main" ]]
    [[ "$output" =~ "array_undefined_element.zig:2:" ]]
}

@test "no error when array element is defined (uniform region model)" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_uniform_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no error when array is initialized with values" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_initialized.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when array element is set to undefined (uniform region model)" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_set_to_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_set_to_undefined.main" ]]
    [[ "$output" =~ "array_set_to_undefined.zig:5:" ]]
    [[ "$output" =~ "undefined value assigned in array_set_to_undefined.main" ]]
    [[ "$output" =~ "array_set_to_undefined.zig:4:" ]]
}

@test "detects undefined struct field in array" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_of_structs_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_of_structs_undefined.main" ]]
    [[ "$output" =~ "array_of_structs_undefined.zig:10:" ]]
}

@test "no error when array of structs element is defined (uniform region model)" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_of_structs_defined.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined pointer in array" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_of_pointers_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_of_pointers_undefined.main" ]]
    [[ "$output" =~ "array_of_pointers_undefined.zig:5:" ]]
}

@test "no error when array of pointers element is defined (uniform region model)" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_of_pointers_defined.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined array element passed to function" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_pass_to_function.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_pass_to_function.main" ]]
    [[ "$output" =~ "array_pass_to_function.zig:9:" ]]
}

@test "detects undefined after multiple assignments (last write wins)" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_multiple_assignments.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_multiple_assignments.main" ]]
    [[ "$output" =~ "array_multiple_assignments.zig:6:" ]]
}

@test "detects undefined when loading array element to local variable" {
    run compile_and_run "$TEST_CASES/undefined/regions/array_load_then_use.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in array_load_then_use.main" ]]
    [[ "$output" =~ "array_load_then_use.zig:5:" ]]
}

# =============================================================================
# Global Variable Tests
# =============================================================================

@test "detects undefined global variable used before assignment" {
    run compile_and_run "$TEST_CASES/undefined/globals/use_undefined_global.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in use_undefined_global.use_global" ]]
    [[ "$output" =~ "use_undefined_global.zig:6:" ]]
}

@test "no error when global is initialized with value" {
    run compile_and_run "$TEST_CASES/undefined/globals/use_defined_global.zig"
    [ "$status" -eq 0 ]
}

@test "no error when undefined global is assigned before use" {
    run compile_and_run "$TEST_CASES/undefined/globals/assign_then_use_global.zig"
    [ "$status" -eq 0 ]
}
