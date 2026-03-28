// Test: allocation in one branch stored into argument's slice field is NOT a leak
// This more closely matches ArrayList.ensureTotalCapacityPrecise pattern
const std = @import("std");

const MyArray = struct {
    items: []u8,
    allocator: std.mem.Allocator,
};

pub fn ensureCapacity(self: *MyArray, new_capacity: usize) !void {
    if (self.items.len >= new_capacity) {
        // Already have enough capacity - do nothing
        return;
    }

    // The pattern from stdlib: try remap, else allocate new
    // Simplified: we just allocate new
    const new_items = try self.allocator.alloc(u8, new_capacity);

    // Copy old data
    if (self.items.len > 0) {
        @memcpy(new_items[0..self.items.len], self.items);
        self.allocator.free(self.items);
    }

    // Store new allocation into argument field
    self.items = new_items;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Start with allocated memory to avoid stack pointer issues
    const initial = allocator.alloc(u8, 1) catch return 1;
    var arr = MyArray{
        .items = initial,
        .allocator = allocator,
    };

    ensureCapacity(&arr, 10) catch return 1;

    // Clean up
    allocator.free(arr.items);

    return 0;
}
