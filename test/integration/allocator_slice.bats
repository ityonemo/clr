#!/usr/bin/env bats

load test_helper

# =============================================================================
# Slice allocator tests (allocator.alloc/free)
# =============================================================================

@test "no false positive for correct slice usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free for slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:9:" ]]
    [[ "$output" =~ "'slice' freed in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:7:" ]]
}

@test "detects double-free for slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:8:" ]]
    [[ "$output" =~ "'slice' previously freed in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:7:" ]]
}

@test "detects memory leak for slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig:8:" ]]
}

@test "no false positive when callee frees slice from caller" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/pass_slice_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free slice from caller" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in pass_to_callee_leak.main" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:13:" ]]
    [[ "$output" =~ "allocated in pass_to_callee_leak.main" ]]
}

@test "detects double-free across caller/callee for slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free across caller/callee for slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/slice/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}

@test "no false positive for freeing slice returned from function" {
    # Tests that freeing a slice returned from a function doesn't report
    # "free of global/comptime memory" - the returned slice may point to
    # dynamically allocated memory, not global memory.
    run compile_and_run "$TEST_CASES/allocator_safety/slice/free_returned_slice.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive when storing slice_ptr into struct field via in-out arg" {
    # Tests that extracting .ptr from allocated slice and storing into struct
    # field via pointer argument doesn't report leak - the allocation escapes
    # via the modified struct.
    run compile_and_run "$TEST_CASES/allocator_safety/slice/slice_ptr_transfer.zig"
    [ "$status" -eq 0 ]
}

@test "detects freeing via pointer arithmetic with optional wrapper" {
    # ptr_sub preserves derived-pointer provenance; it does not prove that the
    # pointer has returned to the allocation base.
    run compile_and_run "$TEST_CASES/allocator_safety/slice/optional_ptr_arithmetic_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "optional_ptr_arithmetic_free.Self.deallocate" ]]
}

@test "detects freeing via simple pointer arithmetic" {
    # ptr_sub over a derived pointer remains derived until a future retag or
    # internal stdlib override reestablishes allocation-base provenance.
    run compile_and_run "$TEST_CASES/allocator_safety/slice/ptr_arithmetic_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "ptr_arithmetic_free.main" ]]
}
