const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate with alloc (slice)
    const slice = allocator.alloc(u8, 10) catch return 1;
    slice[0] = 42;

    // Try to free with destroy (single item) - MISMATCH!
    // Use @ptrCast to bypass type system (simulates a bug)
    // destroy expects a single pointer from create, not a slice
    const ptr: *u8 = @ptrCast(slice.ptr);
    allocator.destroy(ptr);

    return 0;
}
