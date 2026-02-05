// Test: correct nested loop cleanup using errdefer
const std = @import("std");

pub fn main() u8 {
    doWork() catch return 1;
    return 0;
}

fn doWork() !void {
    const allocator = std.heap.page_allocator;

    var i: u8 = 0;
    while (i < 2) : (i += 1) {
        const outer_ptr = try allocator.create(u8);
        errdefer allocator.destroy(outer_ptr); // Clean up if inner loop fails
        outer_ptr.* = i;

        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            const inner_ptr = try allocator.create(u8);
            inner_ptr.* = j;
            allocator.destroy(inner_ptr);
        }

        allocator.destroy(outer_ptr);
    }
}
