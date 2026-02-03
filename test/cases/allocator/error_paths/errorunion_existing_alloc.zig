// Test: Function returning error union - error path should not affect existing allocation
// The allocation should NOT be cleared on the error path because the errorunion
// is from a function call, not from an allocation instruction.
const std = @import("std");

fn maybeModify(x: *u8) !void {
    if (x.* == 0) return error.IsZero;
    x.* += 1;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    // This returns an errorunion from a function (not allocation)
    // On error path, ptr's allocation metadata should NOT be cleared
    maybeModify(ptr) catch {
        // Even on error, ptr is still allocated and must be freed
        allocator.destroy(ptr);
        return 2;
    };

    allocator.destroy(ptr);
    return 0;
}
