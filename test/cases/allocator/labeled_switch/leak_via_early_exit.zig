// Test: memory leak when exiting early from state machine
// Allocate, then exit without freeing
const std = @import("std");

pub fn main() u8 {
    const State = enum { alloc, process, cleanup };
    const allocator = std.heap.page_allocator;
    var ptr: ?*u8 = null;

    state: switch (State.alloc) {
        .alloc => {
            ptr = allocator.create(u8) catch return 1;
            ptr.?.* = 42;
            // Skip cleanup, exit early - LEAK!
            return ptr.?.*;
        },
        .process => {
            continue :state .cleanup;
        },
        .cleanup => {
            if (ptr) |p| allocator.destroy(p);
        },
    }
    return 0;
}
