// Test: use-after-free via dispatch
// case A frees memory, dispatches to case B which uses it
// Should detect use-after-free
const std = @import("std");

pub fn main() u8 {
    const State = enum { free_it, use_it };
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    state: switch (State.free_it) {
        .free_it => {
            allocator.destroy(ptr); // Free the memory
            continue :state .use_it;
        },
        .use_it => {
            return ptr.*; // USE AFTER FREE!
        },
    }
}
