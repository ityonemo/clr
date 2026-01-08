#!/usr/bin/env bats

load test_helper

@test "detects fieldParentPtr on standalone variable" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_standalone.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr on non-field pointer in" ]]
    [[ "$output" =~ "invalid_standalone.main" ]]
}

@test "detects fieldParentPtr with wrong container type" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_wrong_type.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr type mismatch in" ]]
    [[ "$output" =~ "invalid_wrong_type.main" ]]
    # Type names not available, uses type_id.field_name format
    [[ "$output" =~ "pointer is from type" ]]
    [[ "$output" =~ "@fieldParentPtr claims type" ]]
}

@test "detects fieldParentPtr with wrong field" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_wrong_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr field mismatch in" ]]
    [[ "$output" =~ "invalid_wrong_field.main" ]]
    [[ "$output" =~ "pointer is from field x" ]]
    [[ "$output" =~ "@fieldParentPtr claims field y" ]]
}

@test "no error when fieldParentPtr on actual struct field" {
    run compile_and_run "$TEST_CASES/field_safety/valid_struct_field.zig"
    [ "$status" -eq 0 ]
}

@test "no error when fieldParentPtr on actual union field" {
    run compile_and_run "$TEST_CASES/field_safety/valid_union_field.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Global fieldParentPtr tests
# =============================================================================

# KNOWN LIMITATION: Global field pointers are interned at compile time.
# Zig computes &global.field addresses at comptime, so no struct_field_ptr
# instruction is generated. See LIMITATIONS.md for details.
@test "no error when fieldParentPtr on global struct field" {
    skip "Known limitation: global field pointers are interned, no struct_field_ptr generated"
    run compile_and_run "$TEST_CASES/field_safety/globals/valid_struct_field.zig"
    [ "$status" -eq 0 ]
}

@test "detects fieldParentPtr on global standalone variable" {
    skip "Known limitation: global field pointers are interned, no struct_field_ptr generated"
    run compile_and_run "$TEST_CASES/field_safety/globals/invalid_standalone.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr on non-field pointer" ]]
    [[ "$output" =~ "invalid_standalone.get_parent" ]]
}

@test "no error when fieldParentPtr on global union field" {
    skip "Known limitation: global field pointers are interned, no struct_field_ptr generated"
    run compile_and_run "$TEST_CASES/field_safety/globals/valid_union_field.zig"
    [ "$status" -eq 0 ]
}
