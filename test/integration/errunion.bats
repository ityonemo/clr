#!/usr/bin/env bats

load test_helper

# =============================================================================
# Error union tests
# =============================================================================

@test "unwrap_errunion_payload_ptr tracks through pointer modification" {
    run compile_and_run "$TEST_CASES/errunion/payload_ptr.zig"
    [ "$status" -eq 0 ]
}
