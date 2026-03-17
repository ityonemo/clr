// Test: double free across two cases via switch_dispatch
// The pointer is freed in .free_first, then control transfers to .free_second
// which frees it again. Current implementation should miss this.
const std = @import("std");

pub fn main() u8 {
    const State = enum { start, free_first, free_second };
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    state: switch (State.start) {
        .start => {
            continue :state .free_first;
        },
        .free_first => {
            allocator.destroy(ptr); // First free
            continue :state .free_second;
        },
        .free_second => {
            allocator.destroy(ptr); // Second free - DOUBLE FREE!
        },
    }
    return 0;
}
