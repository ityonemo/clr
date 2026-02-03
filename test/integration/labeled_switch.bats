#!/usr/bin/env bats

load test_helper

# =============================================================================
# Labeled Switch (Duff's Device) Tests
# =============================================================================
# Zig's labeled switch with continue enables computed goto-like control flow,
# useful for state machines, tokenizers, and instruction dispatch.

# =============================================================================
# Undefined Safety - Labeled Switch
# =============================================================================

@test "labeled switch - basic state machine defines variable" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/basic_state_machine.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - detects undefined when skipping definition state" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/undefined_skip_state.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "undefined_skip_state.zig" ]]
}

@test "labeled switch - all paths to final state define variable" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/all_paths_define.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - detects undefined use during transition" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/use_in_transition.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use of undefined value" ]]
    [[ "$output" =~ "use_in_transition.zig" ]]
}

@test "labeled switch - tokenizer pattern with multiple paths" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/undefined/labeled_switch/tokenizer_pattern.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "may be undefined" ]]
    [[ "$output" =~ "tokenizer_pattern.zig" ]]
}

# =============================================================================
# Memory Safety - Labeled Switch
# =============================================================================

@test "labeled switch - allocate and free across states" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/alloc_free_states.zig"
    [ "$status" -eq 0 ]
}

@test "labeled switch - detects leak when skipping free state" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/leak_skip_free.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "memory leak" ]]
    [[ "$output" =~ "leak_skip_free.zig" ]]
}

@test "labeled switch - detects double free on state revisit" {
    # SKIP: loop_switch_br tag not implemented
    skip "loop_switch_br tag not implemented"
    run compile_and_run "$TEST_CASES/allocator/labeled_switch/double_free_revisit.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double free" ]]
    [[ "$output" =~ "double_free_revisit.zig" ]]
}
