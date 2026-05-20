#!/usr/bin/env bats

load test_helper

@test "remap: detects UAF after successful remap (if form)" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/uaf_after_remap_if.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "remap: detects UAF after successful remap (orelse form)" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/uaf_after_remap_orelse.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
}

@test "remap: detects double-free after remap" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/double_free_remap.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
}

@test "remap: allows original use on failure path" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/remap_failure_original_valid.zig"
    [ "$status" -eq 0 ]
}

@test "remap: allows new slice use on success path" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/remap_success_use_new.zig"
    [ "$status" -eq 0 ]
}

@test "remap: detects leak when neither freed" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/remap_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
}

@test "remap: allows orelse fallback pattern" {
    run compile_and_run "$TEST_CASES/allocator_safety/remap/remap_orelse_fallback.zig"
    [ "$status" -eq 0 ]
}
