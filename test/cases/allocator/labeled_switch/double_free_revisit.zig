// Test: double free when revisiting a state (simulated loop)
const std = @import("std");

pub fn main() u8 {
    const State = enum { inc_visits, free_ptr };
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var visits: u8 = 0;
    state: switch (State.inc_visits) {
        .inc_visits => {
            visits += 1;
            continue :state .free_ptr;
        },
        .free_ptr => {
            allocator.destroy(ptr); // First visit: OK, second visit: DOUBLE FREE
            if (visits < 2) {
                continue :state .inc_visits; // Loop back
            }
        },
    }
    return 0;
}
