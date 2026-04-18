---
paths:
  - "lib/analysis/fd_safety.zig"
---

# FD Safety Analysis

## Refinement Types with fd_safety

**ALLOWED (may be null or non-null)**:
- `scalar` - file descriptors are integer values

**MUST BE NULL (has analyte, but fd_safety invalid)**:
- `pointer`, `optional`, `errorunion`, `struct`, `union`, `recursive`, `fnptr`, `allocator`, `region`

**NO ANALYTE (no analyte struct exists)**:
- `void`, `noreturn`, `unimplemented`

NOTE: init() functions may leave these invariants temporarily unsatisfied, as long as the tag handler sets the invariants correctly before the tag processing is complete.

## Purpose

Tracks file descriptor state (open/closed). Detects FD leaks and use-after-close.

## States

- **untracked** - not a tracked FD
- **open** - FD is open (with metadata about type and where opened)

The `Open` state includes:
- `meta` - where the FD was opened
- `fd_type` - type of FD (file, socket, pipe, epoll, dup)
- `closed` - null if still open, has Close metadata if closed

## FD Types

- `file` - posix.open/openat
- `socket` - posix.socket/accept
- `pipe` - posix.pipe/pipe2
- `epoll` - posix.epoll_create
- `dup` - posix.dup/dup2

## Key Operations

- Opening an FD sets `.open` state with the appropriate type
- Closing an FD sets the `closed` field within the `Open` state
- At function end, check for open FDs that weren't closed or returned
