// Test case: bitcasting a pointer to scalars ([*]u8) to pointer to structs ([*]Struct)
// This is the pattern HashMap uses - allocate raw bytes, then bitcast to metadata type.
// The bitcast should convert the region element type from scalar to struct.

const std = @import("std");

const Metadata = packed struct {
    fingerprint: u7 = 0,
    used: bool = false,
};

// Simulate what HashMap does: treat raw bytes as structured data
fn asMetadata(bytes: *[4]u8) [*]Metadata {
    // Bitcast from [*]u8 to [*]Metadata - this should convert region element type
    const ptr: [*]u8 = bytes;
    return @ptrCast(ptr);
}

pub fn main() u8 {
    // Stack-allocated bytes (avoids memory leak detection)
    var bytes: [4]u8 = .{ 0, 0, 0, 0 };

    // Get metadata pointer via bitcast
    const metadata = asMetadata(&bytes);

    // Initialize via memset-like operation (store to each element)
    metadata[0] = .{ .fingerprint = 0, .used = false };
    metadata[1] = .{ .fingerprint = 1, .used = true };

    // Access struct fields - should not trigger "use of undefined value"
    // because the store above defined the values
    const result: u8 = if (metadata[0].used) 1 else 0;
    const result2: u8 = if (metadata[1].used) 2 else 0;
    return result + result2;
}
