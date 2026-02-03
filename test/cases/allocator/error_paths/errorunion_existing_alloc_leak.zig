// Test: Function returning error union - forgetting to free on error path is a leak
// This tests that we DON'T incorrectly clear allocation metadata from existing allocations.
const std = @import("std");

fn maybeModify(x: *u8) !void {
    if (x.* == 0) return error.IsZero;
    x.* += 1;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 0; // Will cause maybeModify to return error

    // This returns an errorunion from a function (not allocation)
    // On error path, ptr is still allocated - forgetting to free is a LEAK
    maybeModify(ptr) catch {
        // BUG: We forgot to free ptr here!
        return 2;
    };

    allocator.destroy(ptr);
    return 0;
}
