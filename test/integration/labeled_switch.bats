#!/usr/bin/env bats

load test_helper

# =============================================================================
# Labeled Switch (Duff's Device) Tests
# =============================================================================
# Zig's labeled switch with continue enables computed goto-like control flow,
# useful for state machines, tokenizers, and instruction dispatch.
#
# NOTE: Current implementation analyzes each case independently and merges.
# This causes:
# - False positives: tests expecting success may fail (conservative analysis)
# - False negatives: tests expecting flow-sensitive errors may pass
# - Different error messages: "use of undefined value" vs "may be undefined"
#
# Proper control-flow-sensitive analysis requires tracking state across
# switch_dispatch transitions, which is not yet implemented.

# =============================================================================
# Undefined Safety - Labeled Switch
# =============================================================================

@test "labeled switch - detects undefined when skipping definition state" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/undefined_skip_state.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "undefined_skip_state.zig" ]]
}

@test "labeled switch - all paths to final state define variable" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/all_paths_define.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - detects undefined use during transition" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/use_in_transition.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "use_in_transition.zig" ]]
}

@test "labeled switch - tokenizer pattern with multiple paths" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/tokenizer_pattern.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "tokenizer_pattern.zig" ]]
}

@test "labeled switch - variable defined then used across dispatch" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/defined_then_used_across_dispatch.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - cyclic dispatch converges" {
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/cyclic_dispatch_converges.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Memory Safety - Labeled Switch
# =============================================================================

@test "labeled switch - allocate and free across states" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/alloc_free_states.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - detects leak when skipping free state" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/leak_skip_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "leak_skip_free.zig" ]]
}

@test "labeled switch - detects double free on state revisit" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/double_free_revisit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_revisit.zig" ]]
}

@test "labeled switch - detects double free across cases" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/double_free_across_cases.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_across_cases.zig" ]]
}

@test "labeled switch - detects use after free via dispatch" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/use_after_free_via_dispatch.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after free" ]]
    [[ "$output" =~ "use_after_free_via_dispatch.zig" ]]
}

@test "labeled switch - detects leak via early exit" {
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/leak_via_early_exit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "leak_via_early_exit.zig" ]]
}

# =============================================================================
# Null Safety - Labeled Switch
# =============================================================================

@test "labeled switch - null check propagates across dispatch" {
    run compile_and_run "$TEST_CASES/null/labeled_switch/null_check_across_dispatch.zig"
    [ "$status" -eq 0 ]
}
