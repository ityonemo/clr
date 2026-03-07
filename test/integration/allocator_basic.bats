#!/usr/bin/env bats

load test_helper

# =============================================================================
# Basic memory safety tests
# =============================================================================

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

@test "detects freeing pointer to global variable" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_global_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_ptr.main" ]]
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

# =============================================================================
# Allocator interface coverage tests
# =============================================================================

@test "allocator interface - all methods work correctly" {
    run compile_and_run "$TEST_CASES/allocator/basic/allocator_interface.zig"
    [ "$status" -eq 0 ]
}

@test "detects alloc/destroy method mismatch" {
    run compile_and_run "$TEST_CASES/allocator/basic/alloc_destroy_mismatch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocation method mismatch" ]]
    [[ "$output" =~ "allocated with" ]]
    [[ "$output" =~ "alloc, freed with destroy" ]]
}

@test "detects create/free method mismatch" {
    run compile_and_run "$TEST_CASES/allocator/basic/create_free_mismatch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocation method mismatch" ]]
    [[ "$output" =~ "allocated with" ]]
    [[ "$output" =~ "create, freed with free" ]]
}

@test "detects freeing stack array" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_stack_array.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
    [[ "$output" =~ "free_stack_array.main" ]]
    [[ "$output" =~ "pointer is to local variable" ]]
}

@test "detects freeing slice to stack array" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_stack_slice.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
    [[ "$output" =~ "free_stack_slice.main" ]]
    [[ "$output" =~ "pointer is to local variable" ]]
}

@test "detects freeing sub-slice of allocated slice" {
    run compile_and_run "$TEST_CASES/allocator/basic/free_subslice.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "free_subslice.main" ]]
}

@test "detects pointer arithmetic on single-item pointer" {
    run compile_and_run "$TEST_CASES/allocator/basic/ptr_add_single_item.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "pointer arithmetic on single-item pointer" ]]
    [[ "$output" =~ "ptr_add_single_item.main" ]]
}

# =============================================================================
# Realloc/remap tests
# =============================================================================

@test "no false positive for correct realloc usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/realloc_basic.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-realloc on old slice" {
    run compile_and_run "$TEST_CASES/allocator/basic/realloc_use_after.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "realloc_use_after.main" ]]
}

@test "detects double-free after realloc" {
    run compile_and_run "$TEST_CASES/allocator/basic/realloc_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "realloc_double_free.main" ]]
}

@test "no false positive for correct remap usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/remap_basic.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Dupe/dupeZ tests
# =============================================================================

@test "no false positive for correct dupe usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/dupe_basic.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for correct dupeZ usage" {
    run compile_and_run "$TEST_CASES/allocator/basic/dupeZ_basic.zig"
    [ "$status" -eq 0 ]
}

@test "detects memory leak from dupe" {
    run compile_and_run "$TEST_CASES/allocator/basic/dupe_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "dupe_leak.main" ]]
}

@test "detects use-after-free from dupe" {
    run compile_and_run "$TEST_CASES/allocator/basic/dupe_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "dupe_use_after_free.main" ]]
}

# =============================================================================
# Error Path Tests
# =============================================================================

@test "no false positive for allocation in loop (error path clears phantom allocation)" {
    run compile_and_run "$TEST_CASES/allocator/error_paths/loop_alloc_success.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Cleanup issue tests
# =============================================================================

@test "bitcast preserves memory_safety tracking" {
    run compile_and_run "$TEST_CASES/allocator/bitcast/bitcast_memory.zig"
    [ "$status" -eq 0 ]
}

@test "optional_payload preserves memory_safety tracking" {
    run compile_and_run "$TEST_CASES/allocator/optional/optional_payload_memory.zig"
    [ "$status" -eq 0 ]
}

@test "recursive type memory tracking" {
    run compile_and_run "$TEST_CASES/allocator/recursive/recursive_type_memory.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for conditional optional allocation with destroy" {
    run compile_and_run "$TEST_CASES/allocator/conditional/optional_alloc_destroy.zig"
    [ "$status" -eq 0 ]
}
