#!/usr/bin/env bats

load test_helper

@test "detects use-after-free" {
    run compile_and_run "$TEST_CASES/allocator/basic/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "detects double-free" {
    run compile_and_run "$TEST_CASES/allocator/basic/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "detects memory leak" {
    run compile_and_run "$TEST_CASES/allocator/basic/memory_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "no false positive for correct allocator usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects freeing stack pointer with allocator" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_stack_pointer.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
}

@test "detects mismatched allocator for create/destroy" {
    run compile_and_run "$TEST_CASES/allocator/basic/mismatched_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
}

@test "no false positive when callee frees allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "detects double-free across caller/callee boundary" {
    run compile_and_run "$TEST_CASES/allocator/basic/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "no false positive when caller frees allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_from_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when caller doesn't free allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_from_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}
