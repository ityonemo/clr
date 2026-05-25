#!/usr/bin/env bats

load test_helper

@test "detects reading undefined data from allocated pointer" {
    run compile_and_run "$TEST_CASES/undefined_safety/allocated_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "undefined" ]]
}

@test "handles nested if statements" {
    run compile_and_run "$TEST_CASES/undefined_safety/nested_if.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when early return path doesn't set in-out param" {
    run compile_and_run "$TEST_CASES/undefined_safety/late_set_after_branch.zig"
    [ "$status" -ne 0 ]
    # TODO: Flesh out the error message to be more descriptive
    [[ "$output" =~ "conflicting" ]]
}

@test "no false positive for nested block in cond_br branch" {
    # Tests that nested blocks inside cond_br branches have their bodies expanded.
    # Uses std.mem.indexOfSentinel which triggers SIMD code with nested safety blocks.
    run compile_and_run "$TEST_CASES/null_safety/nested_block_in_cond_br.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.mem.indexOfSentinel SIMD" {
    # Tests that SIMD code in std.mem.indexOfSentinel doesn't trigger false positives.
    # The safety check block inside cond_br branches must have its body properly expanded.
    run compile_and_run "$TEST_CASES/misc/nested_block_optional_simd.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for switch expression initializing struct" {
    # Tests that block bodies execute inline before subsequent instructions.
    # In AIR, switch expression blocks have bodies at high indices that must
    # execute before the block result is used.
    run compile_and_run "$TEST_CASES/misc/block_body_order.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for packed struct initialization" {
    # Packed structs use read-modify-write patterns that load undefined bits
    # before masking and storing. Fields should start as defined to avoid
    # false positives on the initial read.
    run compile_and_run "$TEST_CASES/undefined_safety/structs/packed_struct_init.zig"
    [ "$status" -eq 0 ]
}

@test "detects packed struct undefined state across scalar call boundary" {
    # Packed struct safety must survive conversion to the backing scalar and
    # unpacking in a callee.
    run compile_and_run "$TEST_CASES/misc/packed_struct_scalar_cross_call.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "packed_struct_scalar_cross_call" ]]
}

@test "no false positive for struct store propagation" {
    # Tests that storing a struct into stack storage properly propagates pointer metadata.
    # Pattern from ArrayList.initCapacity: var self = init(); return self;
    # Without struct-to-struct store handling, returned pointer fields would incorrectly
    # retain stack metadata instead of the source's actual metadata.
    run compile_and_run "$TEST_CASES/stack_pointer_safety/struct_store_propagation.zig"
    [ "$status" -eq 0 ]
}

@test "unwrap_errunion_payload_ptr tracks through pointer modification" {
    run compile_and_run "$TEST_CASES/misc/payload_ptr.zig"
    [ "$status" -eq 0 ]
}

@test "error union branch merge handles error-state payloads" {
    run compile_and_run "$TEST_CASES/misc/error_return_merge.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for error path clearing allocation metadata" {
    # When an allocation fails (error path), phantom allocation metadata must be
    # cleared while maintaining valid refinement state (memory_safety set to .unset).
    run compile_and_run "$TEST_CASES/allocator_safety/error_paths/error_path_clear_metadata.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for GPA with error path" {
    # GeneralPurposeAllocator's detectLeaks uses array_elem_val with interned sources.
    # This must be handled correctly.
    run compile_and_run "$TEST_CASES/allocator_safety/error_paths/gpa_error_path.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for storing struct into struct field" {
    # When storing a struct value into a struct field (e.g., return .{ .inner = inner }),
    # the undefined_safety state must be copied from source to destination BEFORE
    # the Store tag handler updates ptr.to to point to the source struct.
    run compile_and_run "$TEST_CASES/undefined_safety/store_struct_field_preserves_state.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for loading allocated slice from struct field" {
    # When loading a slice from a struct field and returning it in a new struct,
    # the slice still points to allocated memory. The pointer VALUE is on the stack,
    # but the POINTEE (the region) should retain its .allocated memory_safety.
    # Stack escape detection checks the pointee, not the pointer itself.
    run compile_and_run "$TEST_CASES/allocator_safety/basic/load_preserves_allocation_tracking.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for slice created from pointer" {
    # When creating a slice from a pointer (like in sliceAsBytes), the new slice
    # should share the same region as the source. Elements should remain defined.
    run compile_and_run "$TEST_CASES/undefined_safety/slice_from_ptr_preserves_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.mem.eql with string literals" {
    # Tests that std.mem.eql works on interned string literals without false positives.
    # This exercises the sliceAsBytes path and early return merging for regions.
    run compile_and_run "$TEST_CASES/std/std_mem_eql_strings.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.ArrayList basic usage" {
    # Tests basic ArrayList operations: append, pop, clear.
    run compile_and_run "$TEST_CASES/std/arraylist_basic.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.HashMap basic usage" {
    # Tests basic HashMap operations: put, get, deinit.
    run compile_and_run "$TEST_CASES/std/hashmap_basic.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for memcpy marking destination as defined" {
    # After @memcpy, the destination region should have the same defined state as the source.
    run compile_and_run "$TEST_CASES/undefined_safety/memcpy_marks_dest_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for memset marking destination as defined" {
    # After @memset, the destination region should be defined (set to the value).
    run compile_and_run "$TEST_CASES/undefined_safety/memset_marks_dest_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for allocation reachable through struct argument" {
    # When a struct argument contains a pointer to allocated memory, the allocation
    # should not be reported as leaked at branch merge points inside the called function.
    # Pattern: HashMap passes self to getIndex, metadata field points to allocated memory.
    run compile_and_run "$TEST_CASES/allocator_safety/struct_pointer_field/struct_arg_allocated_field.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for allocation stored through slice ptr field" {
    # When storing an allocation into a struct slice field via ptr_slice_ptr_ptr,
    # the allocation should be reachable from the original struct at branch merge.
    # Pattern: ArrayList.ensureTotalCapacityPrecise stores new allocation to self.items.ptr
    run compile_and_run "$TEST_CASES/misc/slice_ptr_store_reachable.zig"
    [ "$status" -eq 0 ]
}

@test "no placeholder error for block with nested type in branch" {
    # Block with nested type (pointer→region→scalar) inside a conditional branch
    # should have its memory_safety properly initialized. Previously caused
    # "placeholder in merge - entity not initialized" panic.
    run compile_and_run "$TEST_CASES/misc/block_nested_type_in_branch.zig"
    [ "$status" -eq 0 ]
}

@test "no memory-safety init hole for partially undefined aggregate initialization" {
    # Partially undefined struct/array initializers should still initialize
    # memory_safety on all refinement leaves. The undefined state is separate.
    run compile_and_run "$TEST_CASES/misc/undefined_aggregate_memory_init.zig"
    [ "$status" -eq 0 ]
}
