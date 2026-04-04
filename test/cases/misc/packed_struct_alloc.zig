// Test case: allocating a packed struct and accessing it
// This reproduces the HashMap Metadata issue where the allocated
// packed struct is being typed as scalar instead of struct.

const std = @import("std");

const Metadata = packed struct {
    fingerprint: u7 = 0,
    used: bool = false,
};

pub fn main() u8 {
    // Allocate a slice of packed structs
    const allocator = std.heap.page_allocator;
    const slice = allocator.alloc(Metadata, 4) catch return 1;
    defer allocator.free(slice);

    // Initialize with memset
    @memset(slice, .{ .fingerprint = 0, .used = false });

    // Access a field - should be defined, not undefined
    return if (slice[0].used) 1 else 0;
}
