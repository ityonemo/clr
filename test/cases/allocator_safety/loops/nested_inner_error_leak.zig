// Test: leak detected when inner allocation fails without cleaning up outer
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 2) : (i += 1) {
        const outer_ptr = allocator.create(u8) catch return 1;
        outer_ptr.* = i;

        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            // BUG: if this fails, outer_ptr is leaked!
            const inner_ptr = allocator.create(u8) catch return 1;
            inner_ptr.* = j;
            allocator.destroy(inner_ptr);
        }

        allocator.destroy(outer_ptr);
    }

    return 0;
}
