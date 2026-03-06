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

/// Match ArenaAllocator.allocator calls (returns std.mem.Allocator)
pub fn isArenaAllocator(fqn: []const u8) bool {
    return std.mem.indexOf(u8, fqn, "ArenaAllocator.allocator") != null;
}
