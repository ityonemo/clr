// Test: allocation in one branch stored into argument field is NOT a leak
// This reproduces the pattern from ArrayList.ensureTotalCapacityPrecise where
// remap() succeeds in one branch and alignedAlloc() is used in the other,
// both storing into the same argument field.
const std = @import("std");

pub fn conditionalUpdate(self: *?*u8, condition: bool, allocator: std.mem.Allocator) !void {
    if (condition) {
        // Branch 1: "remap" succeeds - use existing value (no new allocation)
        // self.* stays as-is
    } else {
        // Branch 2: need to allocate new memory and store into arg
        const new_ptr = try allocator.create(u8);
        new_ptr.* = 42;
        self.* = new_ptr;
    }
    // At merge: true branch has old ptr, false branch has new allocation
    // The allocation in false branch is NOT leaked - it's stored in arg
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    var ptr: ?*u8 = null;

    // Call with condition=false to trigger the allocation path
    conditionalUpdate(&ptr, false, allocator) catch return 1;

    // Clean up
    if (ptr) |p| {
        allocator.destroy(p);
    }

    return 0;
}
