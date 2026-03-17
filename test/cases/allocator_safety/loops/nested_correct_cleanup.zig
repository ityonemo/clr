// Correct nested loop pattern - allocate and free in same scope
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 2) : (i += 1) {
        const outer_ptr = allocator.create(u8) catch return 1;
        outer_ptr.* = i;

        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            const inner_ptr = allocator.create(u8) catch {
                allocator.destroy(outer_ptr); // Clean up outer before returning
                return 1;
            };
            inner_ptr.* = j;
            allocator.destroy(inner_ptr); // Clean up inner allocation
        }

        allocator.destroy(outer_ptr); // Clean up outer allocation
    }

    return 0; // Should pass - all memory properly freed
}
