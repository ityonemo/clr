// Test case: memset through struct field pointer, accessed interprocedurally
// This matches the HashMap pattern:
// 1. Container struct has a pointer to a region (metadata)
// 2. initData() memsets the region through the struct pointer
// 3. readData() reads from the region through the struct pointer
// 4. main() calls init then read

const std = @import("std");

const Container = struct {
    data: [*]u8,
    len: usize,
};

pub fn main() u8 {
    // Allocate the region
    const slice = std.heap.page_allocator.alloc(u8, 4) catch return 1;
    defer std.heap.page_allocator.free(slice);

    var container: Container = .{
        .data = slice.ptr,
        .len = 4,
    };

    // Init memsets the region
    initData(&container);

    // Read accesses the same region - should be defined
    return readData(&container);
}

fn initData(c: *Container) void {
    @memset(c.data[0..c.len], 0);
}

fn readData(c: *Container) u8 {
    return c.data[0];
}
