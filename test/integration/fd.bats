#!/usr/bin/env bats

load test_helper

# =============================================================================
# Basic file fd tests (posix.open/close)
# =============================================================================

@test "detects double-close on file fd" {
    run compile_and_run "$TEST_CASES/fd_safety/double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close in double_close.main" ]]
    [[ "$output" =~ "double_close.zig:6:19" ]]
    [[ "$output" =~ "previously closed in double_close.main" ]]
    [[ "$output" =~ "double_close.zig:5:19" ]]
    [[ "$output" =~ "originally opened in double_close.main" ]]
    [[ "$output" =~ "double_close.zig:4:33" ]]
}

@test "detects use-after-close with read" {
    run compile_and_run "$TEST_CASES/fd_safety/use_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "use_after_close.main" ]]
    [[ "$output" =~ "closed in" ]]
    [[ "$output" =~ "opened in" ]]
    [[ "$output" =~ "use_after_close.zig:7:22" ]]
    [[ "$output" =~ "use_after_close.zig:5:19" ]]
    [[ "$output" =~ "use_after_close.zig:4:33" ]]
}

@test "detects use-after-close with write" {
    run compile_and_run "$TEST_CASES/fd_safety/write_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "write_after_close.main" ]]
    [[ "$output" =~ "write_after_close.zig:7:23" ]]
    [[ "$output" =~ "write_after_close.zig:5:19" ]]
    [[ "$output" =~ "write_after_close.zig:4:33" ]]
}

@test "detects file fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/fd_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "fd_leak.main" ]]
    [[ "$output" =~ "opened in" ]]
    [[ "$output" =~ "fd_leak.zig:5:4" ]]
    [[ "$output" =~ "fd_leak.zig:4:33" ]]
}

@test "detects fd leak left live until module finalization" {
    run compile_and_run "$TEST_CASES/fd_safety/dup2_finalizer_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "opened in dup2_finalizer_leak.duplicateToTarget" ]]
    [[ "$output" =~ "dup2_finalizer_leak.zig:4:22" ]]
    [[ "$output" =~ "called from dup2_finalizer_leak.main" ]]
    [[ "$output" =~ "dup2_finalizer_leak.zig:11:25" ]]
    [[ "$output" =~ "error.FdLeak" ]]
}

@test "detects fd leak stored through interned global pointer" {
    run compile_and_run "$TEST_CASES/fd_safety/global_fd_store_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "global_fd_store_leak.main" ]]
    [[ "$output" =~ "opened in" ]]
    [[ "$output" =~ "global_fd_store_leak.zig:6:34" ]]
}

@test "no false positive for correct file fd usage" {
    run compile_and_run "$TEST_CASES/fd_safety/valid_open_close.zig"
    [ "$status" -eq 0 ]
}

@test "no false positive for readToEndAlloc on deferred-close file" {
    run compile_and_run "$TEST_CASES/fd_safety/read_to_end_alloc_valid.zig"
    [ "$status" -eq 0 ]
}

@test "no false double-close when defer runs on fallible and normal exits" {
    run compile_and_run "$TEST_CASES/fd_safety/defer_close_after_error_union_valid.zig"
    [ "$status" -eq 0 ]
}

@test "optional fd supports conditional open and close" {
    run compile_and_run "$TEST_CASES/fd_safety/optional_conditional_open_close_valid.zig"
    [ "$status" -eq 0 ]
}

@test "optional fd supports close followed by clearing optional" {
    run compile_and_run "$TEST_CASES/fd_safety/optional_close_and_clear_valid.zig"
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
    [[ "$output" =~ "socket_double_close.zig:6:19" ]]
    [[ "$output" =~ "socket_double_close.zig:5:19" ]]
    [[ "$output" =~ "socket_double_close.zig:4:35" ]]
}

@test "detects socket fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "socket_leak.main" ]]
    [[ "$output" =~ "socket_leak.zig:5:4" ]]
    [[ "$output" =~ "socket_leak.zig:4:35" ]]
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
    [[ "$output" =~ "dup_double_close.zig:8:19" ]]
    [[ "$output" =~ "dup_double_close.zig:7:19" ]]
    [[ "$output" =~ "dup_double_close.zig:5:33" ]]
}

@test "detects dup'd fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/dup_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "dup_leak.main" ]]
    [[ "$output" =~ "dup_leak.zig:7:4" ]]
    [[ "$output" =~ "dup_leak.zig:5:33" ]]
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
    [[ "$output" =~ "epoll_double_close.zig:6:19" ]]
    [[ "$output" =~ "epoll_double_close.zig:5:19" ]]
    [[ "$output" =~ "epoll_double_close.zig:4:42" ]]
}

@test "detects epoll fd leak" {
    run compile_and_run "$TEST_CASES/fd_safety/epoll_leak.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "fd leak" ]]
    [[ "$output" =~ "epoll_leak.main" ]]
    [[ "$output" =~ "epoll_leak.zig:5:4" ]]
    [[ "$output" =~ "epoll_leak.zig:4:42" ]]
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
    [[ "$output" =~ "return_leak.zig:9:4" ]]
    [[ "$output" =~ "opened in return_leak.openFile" ]]
    [[ "$output" =~ "return_leak.zig:4:29" ]]
    [[ "$output" =~ "called from return_leak.main" ]]
    [[ "$output" =~ "return_leak.zig:8:27" ]]
}

# =============================================================================
# Openat tests (posix.openat)
# =============================================================================

@test "detects double-close on openat fd" {
    run compile_and_run "$TEST_CASES/fd_safety/openat_double_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "double close" ]]
    [[ "$output" =~ "openat_double_close.main" ]]
    [[ "$output" =~ "openat_double_close.zig:7:19" ]]
    [[ "$output" =~ "openat_double_close.zig:6:19" ]]
    [[ "$output" =~ "openat_double_close.zig:5:35" ]]
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
    [[ "$output" =~ "dup2_double_close.zig:10:19" ]]
    [[ "$output" =~ "dup2_double_close.zig:9:19" ]]
    [[ "$output" =~ "dup2_double_close.zig:7:22" ]]
}

# =============================================================================
# Cross-operation tests
# =============================================================================

@test "detects use-after-close on socket with read" {
    run compile_and_run "$TEST_CASES/fd_safety/socket_use_after_close.zig"
    [ "$status" -ne 0 ]
    [[ "$output" =~ "use after close" ]]
    [[ "$output" =~ "socket_use_after_close.main" ]]
    [[ "$output" =~ "socket_use_after_close.zig:7:22" ]]
    [[ "$output" =~ "socket_use_after_close.zig:5:19" ]]
    [[ "$output" =~ "socket_use_after_close.zig:4:35" ]]
}
