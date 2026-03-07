#!/usr/bin/env bats

load test_helper

# =============================================================================
# ArenaAllocator tests
# =============================================================================

@test "arena: no false positive for basic arena usage" {
    run compile_and_run "$TEST_CASES/allocator/arena/basic_usage.zig"
    [ "$status" -eq 0 ]
}

@test "arena: detects arena leak when deinit not called" {
    run compile_and_run "$TEST_CASES/allocator/arena/arena_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "arena leak" ]]
    [[ "$output" =~ "arena_leak.main" ]]
}

@test "arena: no false positive for multiple allocations" {
    run compile_and_run "$TEST_CASES/allocator/arena/multiple_allocs.zig"
    [ "$status" -eq 0 ]
}

@test "arena: detects use-after-deinit" {
    run compile_and_run "$TEST_CASES/allocator/arena/use_after_deinit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "use_after_deinit.main" ]]
}

@test "arena: detects double-deinit" {
    run compile_and_run "$TEST_CASES/allocator/arena/double_deinit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double arena deinit" ]]
    [[ "$output" =~ "double_deinit.main" ]]
}

@test "arena: detects allocation after deinit" {
    run compile_and_run "$TEST_CASES/allocator/arena/alloc_after_deinit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocation from deinited allocator" ]]
    [[ "$output" =~ "alloc_after_deinit.main" ]]
}

@test "arena: allows manual free of arena allocation" {
    run compile_and_run "$TEST_CASES/allocator/arena/manual_free_ok.zig"
    [ "$status" -eq 0 ]
}

@test "arena: detects cross-allocator free (arena alloc, page_allocator free)" {
    run compile_and_run "$TEST_CASES/allocator/arena/cross_allocator_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "cross_allocator_free.main" ]]
}

@test "arena: detects cross-allocator free (page_allocator alloc, arena free)" {
    run compile_and_run "$TEST_CASES/allocator/arena/cross_allocator_create.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "cross_allocator_create.main" ]]
}

@test "arena: no false positive for arena returned from function" {
    run compile_and_run "$TEST_CASES/allocator/arena/laundered_arena.zig"
    [ "$status" -eq 0 ]
}
