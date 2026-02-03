// Test: double free when revisiting a state (simulated loop)
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var visits: u8 = 0;
    state: switch (@as(u8, 0)) {
        0 => {
            visits += 1;
            continue :state 1;
        },
        1 => {
            allocator.destroy(ptr); // First visit: OK, second visit: DOUBLE FREE
            if (visits < 2) {
                continue :state 0; // Loop back
            }
        },
        else => {},
    }
    return 0;
}
