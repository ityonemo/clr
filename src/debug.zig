const std = @import("std");
const compiler = @import("compiler");
const Air = compiler.Air;

// =============================================================================
// Debug Output for DLL Context
// =============================================================================
//
// std.debug.print doesn't work in dynamically loaded .so files because it
// uses compile-time initialized vtables that don't survive DLL relocation.
// This module uses std.fmt.bufPrint (which works - no vtables) combined with
// raw Linux syscalls to write to stderr.
// =============================================================================

/// Format and write to stderr using Linux syscall.
/// Uses a fixed buffer, so output is truncated at 4096 bytes.
pub fn print(comptime fmt: []const u8, args: anytype) void {
    var buf: [4096]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, fmt, args) catch &buf;
    _ = std.os.linux.write(2, slice.ptr, slice.len);
}

/// Dump AIR instructions for a function
pub fn dumpAir(fqn: []const u8, func_index: u32, func_air: *const Air) void {
    print("=== AIR for {s} (index: {d}) ===\n", .{ fqn, func_index });
    const tags = func_air.instructions.items(.tag);
    for (tags) |tag| {
        print("{s}\n", .{@tagName(tag)});
    }
    print("=== end AIR ===\n", .{});
}
