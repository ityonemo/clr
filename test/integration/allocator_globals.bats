#!/usr/bin/env bats

load test_helper

# =============================================================================
# Global pointer tests
# =============================================================================

@test "detects use-after-free through global pointer" {
    run compile_and_run "$TEST_CASES/allocator/globals/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "use_after_free.use_global" ]]
}

@test "detects double-free through global pointer" {
    run compile_and_run "$TEST_CASES/allocator/globals/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free.free_global" ]]
}

@test "detects memory leak through global pointer" {
    run compile_and_run "$TEST_CASES/allocator/globals/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "no false positive for correct global pointer usage" {
    run compile_and_run "$TEST_CASES/allocator/globals/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects free of global memory (direct)" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_global_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_ptr.main" ]]
}

@test "detects free of global memory (laundered through function)" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_global_ptr_laundered.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_ptr_laundered.do_free" ]]
}

@test "detects free of global memory (indirect through function)" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_global_ptr_indirect.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_ptr_indirect.do_free" ]]
}

@test "detects free of global slice" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_global_slice.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_slice.main" ]]
}
