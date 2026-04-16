// Test that GeneralPurposeAllocator works with error paths.
// This triggers array_elem_val with interned source in GPA's detectLeaks.

const std = @import("std");

fn mayAllocate(allocator: std.mem.Allocator, should_fail: bool) !*u8 {
    if (should_fail) {
        return error.AllocationFailed;
    }
    return allocator.create(u8);
}

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Try to allocate, but take the error path
    const result = mayAllocate(allocator, true) catch {
        // This path exercises clearAllocationMetadata and GPA's detectLeaks
        return 0;
    };

    // If we get here (shouldn't), free the allocation
    allocator.destroy(result);
    return 1;
}
