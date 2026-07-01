#!/usr/bin/env bats

load test_helper

# =============================================================================
# Basic memory safety tests
# =============================================================================

@test "detects use-after-free" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "basic_use_after_free.zig:9:4" ]]
    [[ "$output" =~ "basic_use_after_free.zig:7:21" ]]
    [[ "$output" =~ "basic_use_after_free.zig:5:32" ]]
}

@test "detects double-free" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "basic_double_free.zig:9:21" ]]
    [[ "$output" =~ "basic_double_free.zig:7:21" ]]
    [[ "$output" =~ "basic_double_free.zig:5:32" ]]
}

@test "detects memory leak" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/memory_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "memory_leak.zig:8:4" ]]
    [[ "$output" =~ "memory_leak.zig:5:32" ]]
}

@test "no false positive for correct allocator usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects freeing stack pointer with allocator" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_stack_pointer.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
    [[ "$output" =~ "free_stack_pointer.zig:8:21" ]]
    [[ "$output" =~ "free_stack_pointer.zig:4:4" ]]
}

@test "detects freeing pointer to global variable" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_global_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of global/comptime memory" ]]
    [[ "$output" =~ "free_global_ptr.main" ]]
    [[ "$output" =~ "free_global_ptr.zig:8:21" ]]
}

@test "no false positive when callee frees allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free allocation from caller" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "basic_pass_to_callee_leak.zig:13:4" ]]
    [[ "$output" =~ "basic_pass_to_callee_leak.zig:10:32" ]]
}

@test "detects double-free across caller/callee boundary" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/basic_pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "basic_pass_to_callee_double_free.zig:14:21" ]]
    [[ "$output" =~ "basic_pass_to_callee_double_free.consumer" ]]
    [[ "$output" =~ "basic_pass_to_callee_double_free.zig:7:21" ]]
    [[ "$output" =~ "called from basic_pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "basic_pass_to_callee_double_free.zig:12:12" ]]
    [[ "$output" =~ "basic_pass_to_callee_double_free.zig:11:32" ]]
}

@test "no false positive when caller frees allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_from_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when caller doesn't free allocation from callee" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_from_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_from_callee_leak.zig:14:4" ]]
    [[ "$output" =~ "free_from_callee_leak.producer" ]]
    [[ "$output" =~ "free_from_callee_leak.zig:5:32" ]]
    [[ "$output" =~ "called from free_from_callee_leak.main" ]]
    [[ "$output" =~ "free_from_callee_leak.zig:11:24" ]]
}

# =============================================================================
# Allocator interface coverage tests
# =============================================================================

@test "allocator interface - all methods work correctly" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/allocator_interface.zig"
    [ "$status" -eq 0 ]
}

@test "detects alloc/destroy method mismatch" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/alloc_destroy_mismatch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocation method mismatch" ]]
    [[ "$output" =~ "allocated with" ]]
    [[ "$output" =~ "alloc, freed with destroy" ]]
    [[ "$output" =~ "alloc_destroy_mismatch.zig:14:21" ]]
    [[ "$output" =~ "alloc_destroy_mismatch.zig:7:33" ]]
}

@test "detects create/free method mismatch" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/create_free_mismatch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocation method mismatch" ]]
    [[ "$output" =~ "allocated with" ]]
    [[ "$output" =~ "create, freed with free" ]]
    [[ "$output" =~ "create_free_mismatch.zig:14:18" ]]
    [[ "$output" =~ "create_free_mismatch.zig:7:32" ]]
}

@test "detects freeing stack array" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_stack_array.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
    [[ "$output" =~ "free_stack_array.main" ]]
    [[ "$output" =~ "pointer is to local variable" ]]
    [[ "$output" =~ "free_stack_array.zig:8:18" ]]
    [[ "$output" =~ "free_stack_array.zig:4:4" ]]
}

@test "detects freeing slice to stack array" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_stack_slice.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of stack memory" ]]
    [[ "$output" =~ "free_stack_slice.main" ]]
    [[ "$output" =~ "pointer is to local variable" ]]
    [[ "$output" =~ "free_stack_slice.zig:9:18" ]]
    [[ "$output" =~ "free_stack_slice.zig:4:4" ]]
}

@test "detects freeing sub-slice of allocated slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/free_subslice.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "free_subslice.main" ]]
    [[ "$output" =~ "free_subslice.zig:10:18" ]]
    [[ "$output" =~ "free_subslice.zig:8:26" ]]
}

@test "errors on pointer arithmetic over non-region pointer" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/ptr_add_single_item.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "pointer arithmetic on non-region pointer" ]]
    [[ "$output" =~ "ptr_add_single_item.main" ]]
    [[ "$output" =~ "ptr_add_single_item.zig:8:46" ]]
}

# =============================================================================
# Realloc/remap tests
# =============================================================================

@test "no false positive for correct realloc usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/realloc_basic.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-realloc on old slice" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/realloc_use_after.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "realloc_use_after.main" ]]
    [[ "$output" =~ "realloc_use_after.zig:15:9" ]]
    [[ "$output" =~ "realloc_use_after.zig:11:39" ]]
    [[ "$output" =~ "realloc_use_after.zig:7:33" ]]
}

@test "detects double-free after realloc" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/realloc_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "realloc_double_free.main" ]]
    [[ "$output" =~ "realloc_double_free.zig:15:18" ]]
    [[ "$output" =~ "realloc_double_free.zig:11:39" ]]
    [[ "$output" =~ "realloc_double_free.zig:7:33" ]]
}

@test "no false positive for correct remap usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/remap_basic.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Dupe/dupeZ tests
# =============================================================================

@test "no false positive for correct dupe usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/dupe_basic.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for correct dupeZ usage" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/dupeZ_basic.zig"
    [ "$status" -eq 0 ]
}

@test "detects memory leak from dupe" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/dupe_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "dupe_leak.main" ]]
    [[ "$output" =~ "dupe_leak.zig:12:4" ]]
    [[ "$output" =~ "dupe_leak.zig:8:32" ]]
}

@test "detects use-after-free from dupe" {
    run compile_and_run "$TEST_CASES/allocator_safety/basic/dupe_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "dupe_use_after_free.main" ]]
    [[ "$output" =~ "dupe_use_after_free.zig:13:9" ]]
    [[ "$output" =~ "dupe_use_after_free.zig:10:18" ]]
    [[ "$output" =~ "dupe_use_after_free.zig:7:32" ]]
}

# =============================================================================
# Error Path Tests
# =============================================================================

@test "no false positive for allocation in loop (error path clears phantom allocation)" {
    run compile_and_run "$TEST_CASES/allocator_safety/error_paths/loop_alloc_success.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Cleanup issue tests
# =============================================================================

@test "bitcast preserves memory_safety tracking" {
    run compile_and_run "$TEST_CASES/allocator_safety/bitcast/bitcast_memory.zig"
    [ "$status" -eq 0 ]
}

@test "optional_payload preserves memory_safety tracking" {
    run compile_and_run "$TEST_CASES/allocator_safety/optional/optional_payload_memory.zig"
    [ "$status" -eq 0 ]
}

@test "recursive type memory tracking" {
    run compile_and_run "$TEST_CASES/allocator_safety/recursive/recursive_type_memory.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for conditional optional allocation with destroy" {
    run compile_and_run "$TEST_CASES/allocator_safety/conditional/optional_alloc_destroy.zig"
    [ "$status" -eq 0 ]
}
