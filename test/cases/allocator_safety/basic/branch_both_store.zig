// Test: both branches store into same argument field
// One branch allocates, other doesn't - neither should leak
const std = @import("std");

fn getCondition() bool {
    var x: bool = true;
    _ = &x;
    return x;
}

pub fn maybeAlloc(ptr_out: *?*u8, allocator: std.mem.Allocator) !void {
    if (getCondition()) {
        // True branch: no allocation, set to null
        ptr_out.* = null;
    } else {
        // False branch: allocate and store
        const p = try allocator.create(u8);
        p.* = 42;
        ptr_out.* = p;
    }
    // At merge: both branches updated ptr_out
    // The allocation in false branch is NOT a leak - stored in arg
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var result: ?*u8 = null;
    maybeAlloc(&result, allocator) catch return 1;

    // Clean up if allocated
    if (result) |p| {
        allocator.destroy(p);
    }

    return 0;
}
