// Test: updating a slice field in a struct via in-out arg
// Pattern matches ArrayList.ensureTotalCapacityPrecise
const std = @import("std");

const Container = struct {
    items: []u8,
    allocator: std.mem.Allocator,
};

fn remap(current: []u8, new_len: usize) ?[]u8 {
    // Simulated remap - returns null to force allocation path
    _ = current;
    _ = new_len;
    return null;
}

pub fn ensureCapacity(self: *Container, new_capacity: usize) !void {
    const old_slice = self.items;

    if (remap(old_slice, new_capacity)) |new_slice| {
        // Remap succeeded - update pointer
        self.items = new_slice;
    } else {
        // Remap failed - allocate new memory
        const new_items = try self.allocator.alloc(u8, new_capacity);

        // Copy old data
        @memcpy(new_items[0..old_slice.len], old_slice);

        // Free old memory
        self.allocator.free(old_slice);

        // Store new allocation into argument field
        self.items = new_items;
    }
    // At branch merge: true has remapped slice, false has new allocation
    // The new allocation is NOT a leak - it's stored in arg field
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate initial slice
    const initial = allocator.alloc(u8, 4) catch return 1;

    var container = Container{
        .items = initial,
        .allocator = allocator,
    };

    // This should not report a false positive
    ensureCapacity(&container, 16) catch return 1;

    // Clean up
    allocator.free(container.items);

    return 0;
}
