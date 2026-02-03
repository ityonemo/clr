// Test: allocate in one state, free in another - correct pattern
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var ptr: ?*u8 = null;

    state: switch (@as(u8, 0)) {
        0 => {
            ptr = allocator.create(u8) catch return 1;
            ptr.?.* = 42;
            continue :state 1;
        },
        1 => {
            // Use the allocated memory
            const val = ptr.?.*;
            _ = val;
            continue :state 2;
        },
        2 => {
            // Free in final state
            allocator.destroy(ptr.?);
        },
        else => {},
    }
    return 0;
}
