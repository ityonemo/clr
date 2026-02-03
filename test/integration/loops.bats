#!/usr/bin/env bats

load test_helper

# =============================================================================
# Basic Loop Tests
# =============================================================================

@test "simple loop compiles and runs" {
    run compile_and_run "$TEST_CASES/undefined/loops/simple_loop.zig"
    [ "$status" -eq 0 ]
}

@test "variable defined before loop can be used after loop" {
    run compile_and_run "$TEST_CASES/undefined/loops/defined_before_loop.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined variable used inside loop body" {
    run compile_and_run "$TEST_CASES/undefined/loops/undefined_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in undefined_in_loop.main" ]]
    [[ "$output" =~ "undefined_in_loop.zig" ]]
}

@test "detects inconsistent value - defined only in loop that may not execute" {
    run compile_and_run "$TEST_CASES/undefined/loops/defined_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "defined_in_loop.zig" ]]
}

@test "variable defined in loop with break - inconsistent without value tracking" {
    # Without value tracking, analysis doesn't know i=0 < 10 is true,
    # so it considers both "loop runs" and "loop doesn't run" paths.
    run compile_and_run "$TEST_CASES/undefined/loops/loop_with_break.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "loop_with_break.zig" ]]
}

@test "infinite loop with conditional break - variable defined" {
    run compile_and_run "$TEST_CASES/undefined/loops/infinite_loop_break.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# For Loop Tests
# =============================================================================

@test "detects undefined array elements in for loop" {
    # SKIP: add tag not implemented - for loops use add for index iteration
    skip "add tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/loops/for_undefined_array.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value found in for_undefined_array.main" ]]
    [[ "$output" =~ "for_undefined_array.zig" ]]
}

@test "for with index capture - both paths define variable" {
    # SKIP: add tag not implemented - for loops use add for index iteration
    skip "add tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/loops/for_else_with_index.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Loop-Else Tests (for..else and while..else)
# =============================================================================

@test "for..else - variable defined in both break and else paths" {
    # SKIP: add tag not implemented - for loops use add for index iteration
    skip "add tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/loops/for_else_defines_var.zig"
    [ "$status" -eq 0 ]
}

@test "for..else - detects undefined when only else defines variable" {
    # SKIP: add tag not implemented - for loops use add for index iteration
    skip "add tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/loops/for_else_only_else_defines.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "for_else_only_else_defines.zig" ]]
}

@test "while..else - detects leak when only else frees" {
    # SKIP: Loop-else paths not tracked for memory safety - leak on break not detected
    skip "Loop-else memory tracking not implemented"
    run compile_and_run "$TEST_CASES/allocator/loops/while_else_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "while_else_free.zig" ]]
}

@test "while..else - no error when both paths free" {
    # SKIP: Error path analysis not yet implemented - see better-error-analysis.md
    skip "Error path analysis needed - see better-error-analysis.md"
    run compile_and_run "$TEST_CASES/allocator/loops/while_else_both_free.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Nested Loop Tests
# =============================================================================

@test "simple nested loop compiles and runs" {
    run compile_and_run "$TEST_CASES/undefined/loops/nested_loop_simple.zig"
    [ "$status" -eq 0 ]
}

@test "variable defined before nested loops can be used after" {
    run compile_and_run "$TEST_CASES/undefined/loops/nested_defined_outer.zig"
    [ "$status" -eq 0 ]
}

@test "detects inconsistent value - defined in inner loop with break" {
    run compile_and_run "$TEST_CASES/undefined/loops/nested_break_inner.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "nested_break_inner.zig" ]]
}

@test "detects inconsistent value - defined in outer loop before inner break" {
    run compile_and_run "$TEST_CASES/undefined/loops/nested_break_outer.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "nested_break_outer.zig" ]]
}

# =============================================================================
# Memory Safety Loop Tests
# =============================================================================

@test "detects memory leak in loop" {
    run compile_and_run "$TEST_CASES/allocator/loops/leak_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "leak_in_loop.zig" ]]
}

@test "detects double-free when freeing same pointer in loop" {
    run compile_and_run "$TEST_CASES/allocator/loops/double_free_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_loop.main" ]]
    [[ "$output" =~ "double_free_loop.zig:16" ]]
    [[ "$output" =~ "previously freed" ]]
    [[ "$output" =~ "originally allocated" ]]
}

@test "no error when allocating and freeing in same iteration" {
    # SKIP: Error path analysis not yet implemented - see better-error-analysis.md
    # The catch return 1 creates an early_return state with allocation metadata
    # from alloc_create, even though the allocation didn't succeed on the error path
    skip "Error path analysis needed - see better-error-analysis.md"
    run compile_and_run "$TEST_CASES/allocator/loops/alloc_free_each_iteration.zig"
    [ "$status" -eq 0 ]
}

@test "no error when allocating before loop and freeing after" {
    run compile_and_run "$TEST_CASES/allocator/loops/alloc_before_free_after.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when breaking without freeing" {
    run compile_and_run "$TEST_CASES/allocator/loops/break_without_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "break_without_free.zig" ]]
}

@test "detects double-free in nested loops" {
    run compile_and_run "$TEST_CASES/allocator/loops/nested_alloc_inner_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "nested_alloc_inner_free.zig" ]]
}

@test "detects use-after-free across loop iterations" {
    run compile_and_run "$TEST_CASES/allocator/loops/use_after_free_loop.zig"
    [ "$status" -ne 0 ]
    # Could be double-free (destroy on iteration 1) or use-after-free (access on iteration 2)
    # depending on which is detected first
    [[ "$output" =~ "use_after_free_loop.zig" ]]
}

@test "no error with correct nested loop cleanup" {
    # SKIP: Same issue as alloc_free_each_iteration - see better-error-analysis.md
    skip "Error path analysis needed - see better-error-analysis.md"
    run compile_and_run "$TEST_CASES/allocator/loops/nested_correct_cleanup.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Null Safety Loop Tests
# =============================================================================

@test "no error when null-checking optional in loop" {
    run compile_and_run "$TEST_CASES/null/loops/null_check_in_loop.zig"
    [ "$status" -eq 0 ]
}

@test "detects unchecked unwrap in loop" {
    run compile_and_run "$TEST_CASES/null/loops/unchecked_unwrap_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "null" ]]
    [[ "$output" =~ "unchecked_unwrap_in_loop.zig" ]]
}

@test "detects may-be-null after loop sets null" {
    run compile_and_run "$TEST_CASES/null/loops/set_null_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "null" ]]
    [[ "$output" =~ "set_null_in_loop.zig" ]]
}

# =============================================================================
# Variant Safety Loop Tests
# =============================================================================

@test "no error when switching on union in loop" {
    run compile_and_run "$TEST_CASES/variant/loops/switch_in_loop.zig"
    [ "$status" -eq 0 ]
}

@test "detects wrong variant access in loop" {
    run compile_and_run "$TEST_CASES/variant/loops/wrong_variant_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "variant" ]]
    [[ "$output" =~ "wrong_variant_in_loop.zig" ]]
}

@test "detects ambiguous variant after loop changes it" {
    run compile_and_run "$TEST_CASES/variant/loops/variant_change_in_loop.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "variant" ]]
    [[ "$output" =~ "variant_change_in_loop.zig" ]]
}
