#!/usr/bin/env bats

load test_helper

@test "detects reading undefined data from allocated pointer" {
    run compile_and_run "$TEST_CASES/misc/allocated_undefined.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "undefined" ]]
}

@test "handles nested if statements" {
    run compile_and_run "$TEST_CASES/misc/nested_if.zig"
    [ "$status" -eq 0 ]
}

@test "detects undefined when early return path doesn't set in-out param" {
    run compile_and_run "$TEST_CASES/misc/late_set_after_branch.zig"
    [ "$status" -ne 0 ]
    # TODO: Flesh out the error message to be more descriptive
    [[ "$output" =~ "conflicting" ]]
}

@test "no false positive for nested block in cond_br branch" {
    # Tests that nested blocks inside cond_br branches have their bodies expanded.
    # Uses std.mem.indexOfSentinel which triggers SIMD code with nested safety blocks.
    run compile_and_run "$TEST_CASES/misc/nested_block_in_cond_br.zig"
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

@test "no false positive for struct store propagation" {
    # Tests that storing a struct into stack storage properly propagates pointer metadata.
    # Pattern from ArrayList.initCapacity: var self = init(); return self;
    # Without struct-to-struct store handling, returned pointer fields would incorrectly
    # retain stack metadata instead of the source's actual metadata.
    run compile_and_run "$TEST_CASES/misc/struct_store_propagation.zig"
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
    run compile_and_run "$TEST_CASES/misc/error_path_clear_metadata.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for GPA with error path" {
    # GeneralPurposeAllocator's detectLeaks uses array_elem_val with interned sources.
    # This must be handled correctly.
    run compile_and_run "$TEST_CASES/misc/gpa_error_path.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for storing struct into struct field" {
    # When storing a struct value into a struct field (e.g., return .{ .inner = inner }),
    # the undefined_safety state must be copied from source to destination BEFORE
    # the Store tag handler updates ptr.to to point to the source struct.
    run compile_and_run "$TEST_CASES/store_struct_field_preserves_state.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for loading allocated slice from struct field" {
    # When loading a slice from a struct field and returning it in a new struct,
    # the slice still points to allocated memory. The pointer VALUE is on the stack,
    # but the POINTEE (the region) should retain its .allocated memory_safety.
    # Stack escape detection checks the pointee, not the pointer itself.
    run compile_and_run "$TEST_CASES/load_preserves_allocation_tracking.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for slice created from pointer" {
    # When creating a slice from a pointer (like in sliceAsBytes), the new slice
    # should share the same region as the source. Elements should remain defined.
    run compile_and_run "$TEST_CASES/slice_from_ptr_preserves_defined.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for std.mem.eql with string literals" {
    # Tests that std.mem.eql works on interned string literals without false positives.
    # This exercises the sliceAsBytes path and early return merging for regions.
    run compile_and_run "$TEST_CASES/std_mem_eql_strings.zig"
    [ "$status" -eq 0 ]
}
