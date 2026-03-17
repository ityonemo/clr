// Test: allocate in one state, free in another - correct pattern
const std = @import("std");

pub fn main() u8 {
    const State = enum { alloc, use, free };
    const allocator = std.heap.page_allocator;
    var ptr: ?*u8 = null;

    state: switch (State.alloc) {
        .alloc => {
            ptr = allocator.create(u8) catch return 1;
            ptr.?.* = 42;
            continue :state .use;
        },
        .use => {
            // Use the allocated memory
            const val = ptr.?.*;
            _ = val;
            continue :state .free;
        },
        .free => {
            // Free in final state
            allocator.destroy(ptr.?);
        },
    }
    return 0;
}
