#!/usr/bin/env bats

load test_helper

# =============================================================================
# Field pointer free tests
# =============================================================================

@test "detects error when trying to free struct field pointer" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/free_field_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "free of field pointer" ]]
    [[ "$output" =~ "free_field_ptr" ]]
}

@test "no error when freeing parent allocation with field pointer in scope" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/free_parent_ok.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing struct via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/free_via_fieldparentptr_struct.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing union via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/free_via_fieldparentptr_union.zig"
    [ "$status" -eq 0 ]
}

@test "no error when freeing tagged union via fieldParentPtr" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/free_via_fieldparentptr_tagged_union.zig"
    [ "$status" -eq 0 ]
}

@test "use-after-free when accessing field pointer after parent freed" {
    run compile_and_run "$TEST_CASES/allocator_safety/field_ptr/use_after_free_field_ptr.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:13:4)" ]]
    [[ "$output" =~ "'container' freed in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:12:21)" ]]
    [[ "$output" =~ "allocated in use_after_free_field_ptr.main" ]]
    [[ "$output" =~ "use_after_free_field_ptr.zig:9:38)" ]]
}
