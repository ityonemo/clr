#!/usr/bin/env bats

load test_helper

# Undefined safety tests for recursive types

@test "detects undefined field in recursive struct" {
    run compile_and_run "$TEST_CASES/undefined/recursive/undefined_next_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "undefined_next_field.main" ]]
}

@test "detects undefined value via traversal in linked list" {
    run compile_and_run "$TEST_CASES/undefined/recursive/undefined_via_traversal.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "undefined_via_traversal.main" ]]
}

# Memory safety tests for recursive types

@test "detects memory leak in linked list" {
    run compile_and_run "$TEST_CASES/allocator/recursive/leak_linked_list.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "detects double free in linked list" {
    run compile_and_run "$TEST_CASES/allocator/recursive/double_free_list.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "no false positive for correct linked list cleanup" {
    run compile_and_run "$TEST_CASES/allocator/recursive/correct_list_cleanup.zig"
    [ "$status" -eq 0 ]
}

@test "no error when populating and depopulating linked list" {
    run compile_and_run "$TEST_CASES/allocator/recursive/populate_depopulate_list.zig"
    [ "$status" -eq 0 ]
}

@test "detects memory leak when losing root of linked list" {
    run compile_and_run "$TEST_CASES/allocator/recursive/lose_root_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}
