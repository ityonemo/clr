// Minimal test: allocation error handling inside labeled switch
const std = @import("std");

pub fn main() u8 {
    const State = enum { do_alloc, done };
    const allocator = std.heap.page_allocator;

    state: switch (State.do_alloc) {
        .do_alloc => {
            // This catch return should clear the phantom allocation
            const ptr = allocator.create(u8) catch return 1;
            ptr.* = 42;
            allocator.destroy(ptr);
            continue :state .done;
        },
        .done => {},
    }
    return 0;
}
