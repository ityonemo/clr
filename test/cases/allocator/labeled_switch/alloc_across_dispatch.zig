// Test: allocate in one case, free in next case (simple 2-state)
const std = @import("std");

pub fn main() u8 {
    const State = enum { alloc, free };
    const allocator = std.heap.page_allocator;
    var ptr: ?*u8 = null;

    state: switch (State.alloc) {
        .alloc => {
            ptr = allocator.create(u8) catch return 1;
            ptr.?.* = 42;
            continue :state .free;
        },
        .free => {
            allocator.destroy(ptr.?);
        },
    }
    return 0;
}
