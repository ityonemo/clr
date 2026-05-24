const std = @import("std");

/// A struct containing a slice field
const Container = struct {
    items: []u8,
    capacity: usize,
};

/// Simulates ArrayList.moveToUnmanaged pattern:
/// Load a slice from a struct field and return it in a new struct.
fn extractItems(self: *Container) Container {
    // Load self.items - this is a slice pointing to allocated memory
    // The loaded pointer should retain the allocation tracking of the original
    const result: Container = .{
        .items = self.items,
        .capacity = self.capacity,
    };
    return result;
}

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Allocate a slice
    const items = allocator.alloc(u8, 10) catch return 1;
    defer allocator.free(items);

    // Store it in a container
    var container: Container = .{
        .items = items,
        .capacity = 10,
    };

    // Extract and return - this should NOT trigger stack escape
    // because items points to heap memory, not stack
    const extracted = extractItems(&container);
    _ = extracted;

    return 0;
}
