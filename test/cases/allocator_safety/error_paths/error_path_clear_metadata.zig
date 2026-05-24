// Test that error path correctly handles allocation metadata clearing.
// When an allocation fails (error path), the phantom allocation metadata
// must be cleared while still maintaining valid refinement state.

const std = @import("std");

fn mayAllocate(allocator: std.mem.Allocator, should_fail: bool) !*u8 {
    if (should_fail) {
        return error.AllocationFailed;
    }
    return allocator.create(u8);
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Try to allocate, but take the error path
    const result = mayAllocate(allocator, true) catch {
        // This path exercises clearAllocationMetadata
        return 0;
    };

    // If we get here (shouldn't), free the allocation
    allocator.destroy(result);
    return 1;
}
