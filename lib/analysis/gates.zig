//! FQN Pattern Matching Gates
//!
//! Shared helper functions for matching stdlib function FQNs in runtime call filters.
//! Used by analysis modules (memory_safety, undefined_safety, etc.) to determine
//! which calls to intercept.

const std = @import("std");

// =========================================================================
// Allocator Operations
// =========================================================================

/// Match mem.Allocator.create calls
pub fn isAllocatorCreate(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.create") != null;
}

/// Match mem.Allocator.destroy calls
pub fn isAllocatorDestroy(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.destroy") != null;
}

/// Match mem.Allocator.alloc, dupe, dupeZ calls (but NOT alignedAlloc)
pub fn isAllocatorAlloc(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "mem.Allocator.alignedAlloc") != null) return false;
    return std.mem.indexOf(u8, fqn, "mem.Allocator.alloc") != null or
        std.mem.indexOf(u8, fqn, "mem.Allocator.dupe") != null;
}

/// Match mem.Allocator.free calls
pub fn isAllocatorFree(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.free") != null;
}

/// Match mem.Allocator.alignedAlloc calls
pub fn isAllocatorAlignedAlloc(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.alignedAlloc") != null;
}

/// Match mem.Allocator.resize calls
pub fn isAllocatorResize(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.resize") != null;
}

/// Match mem.Allocator.realloc calls
pub fn isAllocatorRealloc(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.realloc") != null;
}

/// Match mem.Allocator.remap calls
pub fn isAllocatorRemap(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "mem.Allocator.remap") != null;
}

// =========================================================================
// Arena Operations
// =========================================================================

/// Match ArenaAllocator.init calls
pub fn isArenaInit(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.init") != null;
}

/// Match ArenaAllocator.deinit calls
pub fn isArenaDeinit(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.deinit") != null;
}

/// Match GeneralPurposeAllocator/DebugAllocator.deinit calls
/// GPA wraps DebugAllocator internally, so the FQN is:
/// heap.debug_allocator.DebugAllocator(.{...}).deinit
pub fn isGpaDeinit(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "DebugAllocator(") != null and
        std.mem.indexOf(u8, fqn, ").deinit") != null;
}

/// Match ArenaAllocator.allocator calls (returns std.mem.Allocator)
pub fn isArenaAllocator(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.allocator") != null;
}

// =========================================================================
// File Descriptor Operations (posix)
// =========================================================================

/// Match posix.open calls (but NOT openat)
pub fn isPosixOpen(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "posix.openat") != null) return false;
    return std.mem.indexOf(u8, fqn, "posix.open") != null;
}

/// Match posix.openat calls
pub fn isPosixOpenat(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.openat") != null;
}

/// Match posix.close calls
pub fn isPosixClose(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.close") != null;
}

/// Match posix.socket calls
pub fn isPosixSocket(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.socket") != null;
}

/// Match posix.accept calls
pub fn isPosixAccept(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.accept") != null;
}

/// Match posix.pipe calls
pub fn isPosixPipe(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.pipe") != null;
}

/// Match posix.dup calls (but NOT dup2)
pub fn isPosixDup(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "posix.dup2") != null) return false;
    return std.mem.indexOf(u8, fqn, "posix.dup") != null;
}

/// Match posix.dup2 calls
pub fn isPosixDup2(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.dup2") != null;
}

/// Match posix.epoll_create calls
pub fn isPosixEpollCreate(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.epoll_create") != null;
}

/// Match posix.read calls
pub fn isPosixRead(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "posix.pread") != null) return false;
    return std.mem.indexOf(u8, fqn, "posix.read") != null;
}

/// Match posix.write calls
pub fn isPosixWrite(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "posix.pwrite") != null) return false;
    return std.mem.indexOf(u8, fqn, "posix.write") != null;
}

/// Match posix.pread calls
pub fn isPosixPread(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.pread") != null;
}

/// Match posix.pwrite calls
pub fn isPosixPwrite(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "posix.pwrite") != null;
}

// =========================================================================
// Formatter Functions
// =========================================================================

/// Match fmt.format calls
pub fn isFmtFormat(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "fmt.format") != null;
}

/// Match fmt.bufPrint calls (but NOT bufPrintZ)
pub fn isFmtBufPrint(fqn: []const u8) bool {
    if (std.mem.indexOf(u8, fqn, "fmt.bufPrintZ") != null) return false;
    return std.mem.indexOf(u8, fqn, "fmt.bufPrint") != null;
}

/// Match fmt.bufPrintZ calls
pub fn isFmtBufPrintZ(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "fmt.bufPrintZ") != null;
}

/// Match fmt.count calls
pub fn isFmtCount(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "fmt.count") != null;
}

/// Match log.scoped calls (std.log.scoped(...).debug/info/warn/err)
pub fn isLogScoped(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "log.scoped") != null;
}

/// Match log.default calls (std.log.default.debug/info/warn/err)
pub fn isLogDefault(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "log.default") != null;
}

/// Check if FQN matches any formatter function
pub fn isFormatter(fqn: []const u8) bool {
    return isFmtFormat(fqn) or
        isFmtBufPrint(fqn) or
        isFmtBufPrintZ(fqn) or
        isFmtCount(fqn) or
        isLogScoped(fqn) or
        isLogDefault(fqn);
}
