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

# Recursive union tests

@test "no error when recursive union field is defined" {
    run compile_and_run "$TEST_CASES/undefined/recursive/undefined_union_field.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined value via recursive union pointer" {
    run compile_and_run "$TEST_CASES/undefined/recursive/undefined_union_recursive_field.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
}

@test "no false positive for correct expression tree cleanup" {
    run compile_and_run "$TEST_CASES/allocator/recursive/expr_tree_correct.zig"
    [ "$status" -eq 0 ]
}

@test "detects memory leak in expression tree" {
    run compile_and_run "$TEST_CASES/allocator/recursive/expr_tree_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "detects double free in expression tree" {
    run compile_and_run "$TEST_CASES/allocator/recursive/expr_tree_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

# =============================================================================
# Intrusive Linked List Tests (std.SinglyLinkedList wrapper)
# =============================================================================

@test "intrusive linked list - detects memory leak" {
    run compile_and_run "$TEST_CASES/allocator/intrusive_linked_list/leak_linked_list.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "intrusive linked list - detects double free" {
    run compile_and_run "$TEST_CASES/allocator/intrusive_linked_list/double_free_list.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "intrusive linked list - correct cleanup" {
    run compile_and_run "$TEST_CASES/allocator/intrusive_linked_list/correct_list_cleanup.zig"
    [ "$status" -eq 0 ]
}
