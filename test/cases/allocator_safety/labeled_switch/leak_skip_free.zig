// Test: memory leak when state with free is skipped
const std = @import("std");

pub fn main() u8 {
    const State = enum { alloc, free, end };
    const allocator = std.heap.page_allocator;

    state: switch (State.alloc) {
        .alloc => {
            const ptr = allocator.create(u8) catch return 1;
            ptr.* = 42;
            // Skip free state, go directly to end
            continue :state .end;
        },
        .free => {
            // This state would free but is never reached
            // (Would need ptr passed somehow - simplified test)
        },
        .end => {
            // Leak! ptr allocated in alloc state was never freed
        },
    }
    return 0;
}
