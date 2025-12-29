#!/usr/bin/env bats

load test_helper

@test "detects fieldParentPtr on standalone variable" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_standalone.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr on non-field pointer in" ]]
    [[ "$output" =~ "invalid_standalone.main" ]]
    [[ "$output" =~ "expected pointer from Container.value field" ]]
}

@test "detects fieldParentPtr with wrong container type" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_wrong_type.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr type mismatch in" ]]
    [[ "$output" =~ "invalid_wrong_type.main" ]]
    [[ "$output" =~ "pointer is from ContainerA.x" ]]
    [[ "$output" =~ "@fieldParentPtr claims ContainerB.x" ]]
}

@test "detects fieldParentPtr with wrong field" {
    run compile_and_run "$TEST_CASES/field_safety/invalid_wrong_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fieldParentPtr field mismatch in" ]]
    [[ "$output" =~ "invalid_wrong_field.main" ]]
    [[ "$output" =~ "pointer is from Container.x" ]]
    [[ "$output" =~ "@fieldParentPtr claims Container.y" ]]
}

@test "no error when fieldParentPtr on actual struct field" {
    run compile_and_run "$TEST_CASES/field_safety/valid_struct_field.zig"
    [ "$status" -eq 0 ]
}

@test "no error when fieldParentPtr on actual union field" {
    run compile_and_run "$TEST_CASES/field_safety/valid_union_field.zig"
    [ "$status" -eq 0 ]
}
