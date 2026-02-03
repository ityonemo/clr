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

# Struct pointer field tests

@test "detects use-after-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:17:" ]]
    [[ "$output" =~ "'container.ptr' freed in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' allocated in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:11:" ]]
}

@test "detects double-free through struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:14:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:11:" ]]
}

@test "detects memory leak in struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig:15:" ]]
    [[ "$output" =~ "'container.ptr' allocated in leak.main" ]]
    [[ "$output" =~ "leak.zig:11:" ]]
}

@test "no false positive for correct struct pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive when callee frees struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in pass_to_callee_leak.useContainer" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:9:" ]]
    [[ "$output" =~ "'container.ptr' allocated in pass_to_callee_leak.main" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig:16:" ]]
}

@test "detects double-free when caller frees after callee freed struct pointer field" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:19:" ]]
    [[ "$output" =~ "'container.ptr' previously freed in pass_to_callee_double_free.freeContainer" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:8:" ]]
    [[ "$output" =~ "'container.ptr' originally allocated in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig:15:" ]]
}

@test "detects use-after-free when caller uses struct pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator/struct_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:21:" ]]
    [[ "$output" =~ "'container.ptr' freed in pass_to_callee_use_after_free.freeContainer" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:8:" ]]
    [[ "$output" =~ "'container.ptr' allocated in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig:15:" ]]
}

# =============================================================================
# Union pointer field tests
# =============================================================================

@test "no false positive for correct union pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig" ]]
}

@test "detects double-free through union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig" ]]
}

@test "detects memory leak in union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig" ]]
}

@test "no false positive when callee frees union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free when caller frees after callee freed union pointer field" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free when caller uses union pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}

@test "no false positive for heap pointer in union return" {
    run compile_and_run "$TEST_CASES/allocator/union_pointer_field/no_escape_heap_ptr.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Array pointer field tests
# =============================================================================

@test "no false positive for correct array pointer field usage" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free through array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig" ]]
}

@test "detects double-free through array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig" ]]
}

@test "detects memory leak in array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig" ]]
}

@test "no false positive when callee frees array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free when caller frees after callee freed array pointer field" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free when caller uses array pointer field after callee freed" {
    run compile_and_run "$TEST_CASES/allocator/array_pointer_field/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
}

# =============================================================================
# Clobber leak detection tests
# =============================================================================

@test "detects leak when variable is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/variable_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "variable_clobber" ]]
}

@test "detects leak when struct field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/struct_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "struct_field_clobber" ]]
}

@test "detects leak when union variant changes" {
    run compile_and_run "$TEST_CASES/allocator/clobber/union_variant_change.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_variant_change" ]]
}

@test "detects leak when union field is clobbered" {
    run compile_and_run "$TEST_CASES/allocator/clobber/union_field_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "union_field_clobber" ]]
}

@test "detects leak when variable is clobbered in branch" {
    run compile_and_run "$TEST_CASES/allocator/clobber/branch_clobber.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "branch_clobber" ]]
}

# =============================================================================
# If/branch memory safety tests
# =============================================================================

@test "detects leak when only one if branch frees" {
    run compile_and_run "$TEST_CASES/allocator/if/free_one_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_branch" ]]
}

@test "no error when both if branches free" {
    run compile_and_run "$TEST_CASES/allocator/if/free_both_branches.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when both if branches free same allocation" {
    run compile_and_run "$TEST_CASES/allocator/if/double_free_branches.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_branches" ]]
}

@test "detects leak when variable is clobbered in if branch" {
    run compile_and_run "$TEST_CASES/allocator/if/clobber_in_branch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_branch" ]]
}

@test "no false positive when if branch frees and returns" {
    run compile_and_run "$TEST_CASES/allocator/if/free_and_return.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Switch memory safety tests
# =============================================================================

@test "detects leak when only one switch case frees" {
    run compile_and_run "$TEST_CASES/allocator/switch/free_one_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "free_one_case" ]]
}

@test "no error when all switch cases free" {
    run compile_and_run "$TEST_CASES/allocator/switch/free_all_cases.zig"
    [ "$status" -eq 0 ]
}

@test "detects double free when multiple switch cases free same allocation" {
    run compile_and_run "$TEST_CASES/allocator/switch/double_free_cases.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_cases" ]]
}

@test "detects leak when variable is clobbered in switch case" {
    run compile_and_run "$TEST_CASES/allocator/switch/clobber_in_case.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "clobber_in_case" ]]
}

@test "no false positive when switch case frees and returns" {
    run compile_and_run "$TEST_CASES/allocator/switch/free_and_return.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Field pointer free tests
# =============================================================================

@test "detects error when trying to free struct field pointer" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_field_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "free_field_ptr" ]]
}

@test "no error when freeing parent allocation with field pointer in scope" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_parent_ok.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing struct via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_via_fieldparentptr_struct.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing union via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_via_fieldparentptr_union.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing tagged union via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/free_via_fieldparentptr_tagged_union.zig"
    [ "$status" -eq 0 ]
}

@test "use-after-free when accessing field pointer after parent freed" {
    run compile_and_run "$TEST_CASES/allocator/field_ptr/use_after_free_field_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:13:4)" ]]
    [[ "$output" =~ "'container' freed in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:12:21)" ]]
    [[ "$output" =~ "allocated in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:9:38)" ]]
}

# =============================================================================
# Slice allocator tests (allocator.alloc/free)
# =============================================================================

@test "no false positive for correct slice usage" {
    run compile_and_run "$TEST_CASES/allocator/slice/correct_usage.zig"
    [ "$status" -eq 0 ]
}

@test "detects use-after-free for slice" {
    run compile_and_run "$TEST_CASES/allocator/slice/use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:9:" ]]
    [[ "$output" =~ "'slice' freed in use_after_free.main" ]]
    [[ "$output" =~ "use_after_free.zig:7:" ]]
}

@test "detects double-free for slice" {
    run compile_and_run "$TEST_CASES/allocator/slice/double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:8:" ]]
    [[ "$output" =~ "'slice' previously freed in double_free.main" ]]
    [[ "$output" =~ "double_free.zig:7:" ]]
}

@test "detects memory leak for slice" {
    run compile_and_run "$TEST_CASES/allocator/slice/leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in leak.main" ]]
    [[ "$output" =~ "leak.zig:8:" ]]
}

@test "no false positive when callee frees slice from caller" {
    run compile_and_run "$TEST_CASES/allocator/slice/pass_to_callee_noleak.zig"
    [ "$status" -eq 0 ]
}

@test "detects leak when callee doesn't free slice from caller" {
    run compile_and_run "$TEST_CASES/allocator/slice/pass_to_callee_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak in pass_to_callee_leak.consumer" ]]
    [[ "$output" =~ "pass_to_callee_leak.zig" ]]
}

@test "detects double-free across caller/callee for slice" {
    run compile_and_run "$TEST_CASES/allocator/slice/pass_to_callee_double_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free in pass_to_callee_double_free.main" ]]
    [[ "$output" =~ "pass_to_callee_double_free.zig" ]]
}

@test "detects use-after-free across caller/callee for slice" {
    run compile_and_run "$TEST_CASES/allocator/slice/pass_to_callee_use_after_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in pass_to_callee_use_after_free.main" ]]
    [[ "$output" =~ "pass_to_callee_use_after_free.zig" ]]
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

# =============================================================================
# Mismatched allocator type tests
# =============================================================================

@test "detects mismatched allocator: comptime page_allocator vs c_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/comptime_page_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "comptime_page_allocator.main" ]]
}

@test "detects mismatched allocator: global GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/global_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "global_gpa.main" ]]
}

@test "detects mismatched allocator: local GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/local_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_gpa.main" ]]
}

@test "detects mismatched allocator: local inline GPA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/local_inline_gpa.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_inline_gpa.main" ]]
}

@test "detects mismatched allocator: local FBA vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/local_fba.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "local_fba.main" ]]
}

@test "detects mismatched allocator: passed allocator vs page_allocator" {
    run compile_and_run "$TEST_CASES/allocator/mismatched/passed_allocator.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "allocator mismatch" ]]
    [[ "$output" =~ "passed_allocator" ]]
}

# =============================================================================
# Error Path Tests - Allocation-derived errorunions clear metadata on error path
# =============================================================================

@test "no false positive for allocation in loop (error path clears phantom allocation)" {
    run compile_and_run "$TEST_CASES/allocator/error_paths/loop_alloc_success.zig"
    [ "$status" -eq 0 ]
}
