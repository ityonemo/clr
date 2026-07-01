const std = @import("std");

// Tests that early return in a branch prevents false positive double-free.
// The true branch frees and returns, so the post-if free only runs on false branch.
// This is NOT a double-free - it's correct code.
pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var condition: bool = true;
    _ = &condition;

    if (condition) {
        allocator.destroy(ptr);
        return 0; // Early return - post-if code is unreachable from this path
    }

    // Only reached from false branch where ptr is still live
    allocator.destroy(ptr);
    return 0;
}
