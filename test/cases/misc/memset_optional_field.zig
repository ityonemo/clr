// Test case: memset through optional pointer field in struct passed by pointer
// This matches the HashMap pattern where:
// 1. HashMap has metadata: ?[*]Metadata field
// 2. allocate() sets metadata to point to allocated memory
// 3. initMetadatas() memsets the metadata array
// 4. put/get access the metadata

const std = @import("std");

const Container = struct {
    metadata: ?[*]u8,
    size: u32,
};

pub fn main() u8 {
    var container: Container = .{
        .metadata = null,
        .size = 0,
    };

    // Allocate and initialize through pointer
    const slice = std.heap.page_allocator.alloc(u8, 4) catch return 1;
    defer std.heap.page_allocator.free(slice);

    container.metadata = slice.ptr;
    container.size = 4;

    initMetadata(&container);

    // After init, metadata should be defined
    if (container.metadata) |meta| {
        return meta[0];
    }
    return 1;
}

fn initMetadata(c: *Container) void {
    // Memset through the pointer from the struct field
    if (c.metadata) |meta| {
        @memset(meta[0..c.size], 0);
    }
}
