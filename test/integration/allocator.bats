#!/usr/bin/env bats

load test_helper

@test "detects use-after-free" {
    run compile_and_run "$TEST_CASES/allocator/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "detects double-free" {
    run compile_and_run "$TEST_CASES/allocator/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "detects memory leak" {
    run compile_and_run "$TEST_CASES/allocator/memory_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "no false positive for correct allocator usage" {
    run compile_and_run "$TEST_CASES/allocator/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects freeing stack pointer with allocator" {
    run compile_and_run "$TEST_CASES/allocator/free_stack_pointer.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
}
