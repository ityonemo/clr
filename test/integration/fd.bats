#!/usr/bin/env bats

load test_helper

# =============================================================================
# Basic file fd tests (posix.open/close)
# =============================================================================

@test "detects double-close on file fd" {
    run compile_and_run "$TEST_CASES/fd_safety/double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "double_close.main" ]]
    [[ "$output" =~ "previously closed" ]]
    [[ "$output" =~ "originally opened" ]]
}

@test "detects use-after-close with read" {
    run compile_and_run "$TEST_CASES/fd_safety/use_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "use_after_close.main" ]]
    [[ "$output" =~ "closed in" ]]
    [[ "$output" =~ "opened in" ]]
}

@test "detects use-after-close with write" {
    run compile_and_run "$TEST_CASES/fd_safety/write_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "write_after_close.main" ]]
}

@test "detects file fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/fd_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "fd_leak.main" ]]
    [[ "$output" =~ "opened in" ]]
}

@test "no false positive for correct file fd usage" {
    run compile_and_run "$TEST_CASES/fd_safety/valid_open_close.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Socket fd tests (posix.socket)
# =============================================================================

@test "detects double-close on socket fd" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "socket_double_close.main" ]]
}

@test "detects socket fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "socket_leak.main" ]]
}

@test "no false positive for correct socket usage" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_valid.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Dup fd tests (posix.dup)
# =============================================================================

@test "detects double-close on dup'd fd" {
    run compile_and_run "$TEST_CASES/fd_safety/dup_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "dup_double_close.main" ]]
}

@test "detects dup'd fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/dup_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "dup_leak.main" ]]
}

@test "no false positive for correct dup usage (both fds closed)" {
    run compile_and_run "$TEST_CASES/fd_safety/dup_valid.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Epoll fd tests (posix.epoll_create)
# =============================================================================

@test "detects double-close on epoll fd" {
    run compile_and_run "$TEST_CASES/fd_safety/epoll_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "epoll_double_close.main" ]]
}

@test "detects epoll fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/epoll_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "epoll_leak.main" ]]
}

@test "no false positive for correct epoll usage" {
    run compile_and_run "$TEST_CASES/fd_safety/epoll_valid.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Return propagation tests
# =============================================================================

@test "no false positive when fd is returned to caller who closes it" {
    run compile_and_run "$TEST_CASES/fd_safety/return_no_leak.zig"
    [ "$status" -eq 0 ]
}

@test "detects fd leak when caller doesn't close returned fd" {
    run compile_and_run "$TEST_CASES/fd_safety/return_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "return_leak.main" ]]
}

# =============================================================================
# Openat tests (posix.openat)
# =============================================================================

@test "detects double-close on openat fd" {
    run compile_and_run "$TEST_CASES/fd_safety/openat_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "openat_double_close.main" ]]
}

@test "no false positive for correct openat usage" {
    run compile_and_run "$TEST_CASES/fd_safety/openat_valid.zig"
    [ "$status" -eq 0 ]
}

# =============================================================================
# Dup2 tests (posix.dup2)
# =============================================================================

@test "detects double-close on dup2'd fd" {
    run compile_and_run "$TEST_CASES/fd_safety/dup2_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "dup2_double_close.main" ]]
}

# =============================================================================
# Cross-operation tests
# =============================================================================

@test "detects use-after-close on socket with read" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_use_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "socket_use_after_close.main" ]]
}
