// Test: memory leak when state with free is skipped
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    state: switch (@as(u8, 0)) {
        0 => {
            const ptr = allocator.create(u8) catch return 1;
            ptr.* = 42;
            // Skip state 1 which would free, go directly to 2
            continue :state 2;
        },
        1 => {
            // This state would free but is never reached
            // (Would need ptr passed somehow - simplified test)
        },
        2 => {
            // Leak! ptr allocated in state 0 was never freed
        },
        else => {},
    }
    return 0;
}
