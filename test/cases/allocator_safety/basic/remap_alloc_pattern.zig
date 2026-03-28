// Test: remap/alloc pattern - one branch has no allocation, other branch allocates
// and stores into argument field. This matches ArrayList.ensureTotalCapacityPrecise.
const std = @import("std");

const MyArray = struct {
    ptr: [*]u8,
    len: usize,
    allocator: std.mem.Allocator,
};

fn remap(arr: *MyArray, new_len: usize) ?[*]u8 {
    // Simulate remap - sometimes succeeds, sometimes fails
    // Return null to indicate "remap failed" condition
    _ = arr;
    _ = new_len;
    return null;
}

pub fn ensureCapacity(self: *MyArray, new_capacity: usize) !void {
    // Pattern: try remap first, fall back to alloc
    if (remap(self, new_capacity)) |new_ptr| {
        // Remap succeeded - update pointer (may be same or different)
        self.ptr = new_ptr;
    } else {
        // Remap failed - allocate new memory
        const new_slice = try self.allocator.alloc(u8, new_capacity);
        // Copy old data
        @memcpy(new_slice[0..self.len], self.ptr[0..self.len]);
        // Free old
        self.allocator.free(self.ptr[0..self.len]);
        // Store new allocation into argument field
        self.ptr = new_slice.ptr;
    }
    // At merge point: true branch has (possibly same) ptr, false branch has new allocation
    // Neither should be flagged as a leak
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const initial = allocator.alloc(u8, 1) catch return 1;
    var arr = MyArray{
        .ptr = initial.ptr,
        .len = 1,
        .allocator = allocator,
    };

    ensureCapacity(&arr, 10) catch return 1;

    // Clean up
    allocator.free(arr.ptr[0..arr.len]);

    return 0;
}
