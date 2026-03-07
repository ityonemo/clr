#!/usr/bin/env bats

load test_helper

# =============================================================================
# Function pointer tests
# =============================================================================

@test "detects use-after-free through function pointer" {
    run compile_and_run "$TEST_CASES/allocator/fnptr/use_after_free_fnptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "detects double-free through function pointer" {
    run compile_and_run "$TEST_CASES/allocator/fnptr/double_free_fnptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "detects use-after-free in conditional fnptr branch" {
    run compile_and_run "$TEST_CASES/allocator/fnptr/conditional_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "detects double-free in conditional fnptr branch" {
    run compile_and_run "$TEST_CASES/allocator/fnptr/conditional_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "detects use-after-free through vtable function pointer" {
    run compile_and_run "$TEST_CASES/allocator/fnptr/vtable_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}
