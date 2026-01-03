const std = @import("std");

// Tests that early return in a switch case prevents false positive double-free.
// Case 0 frees and returns, so the post-switch free only runs on other cases.
// This is NOT a double-free - it's correct code.
pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var value: u8 = 0;
    _ = &value;

    switch (value) {
        0 => {
            allocator.destroy(ptr);
            return 0; // Early return - post-switch code is unreachable from this path
        },
        else => {},
    }

    // Only reached from non-zero cases where ptr is still live
    allocator.destroy(ptr);
    return 0;
}
