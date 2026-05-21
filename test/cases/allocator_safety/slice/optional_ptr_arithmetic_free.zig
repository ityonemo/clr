// Test case: freeing via pointer arithmetic with optional pointer.
// Keep this case focused on optional unwrap + ptr_add/ptr_sub provenance; do
// not use @ptrCast(@alignCast(...)) here, because that lowers to a separate
// pointer-address alignment check.

const std = @import("std");

const Self = struct {
    metadata: ?[*]u8 = null,
    capacity_value: usize = 0,

    fn header(self: Self) [*]u8 {
        return self.metadata.? - 1;
    }

    fn capacity(self: Self) usize {
        if (self.metadata == null) return 0;
        return self.capacity_value;
    }

    fn allocate(self: *Self, allocator: std.mem.Allocator, new_capacity: usize) !void {
        const total_size = 1 + new_capacity;
        const slice = try allocator.alloc(u8, total_size);
        const ptr: [*]u8 = slice.ptr;

        ptr[0] = 0;
        const metadata_ptr = ptr + 1;
        self.metadata = metadata_ptr;
        self.capacity_value = new_capacity;
    }

    fn deallocate(self: *Self, allocator: std.mem.Allocator) void {
        if (self.metadata == null) return;

        const cap = self.capacity();
        const total_size = 1 + cap;

        const slice = self.header()[0..total_size];
        allocator.free(slice);

        self.metadata = null;
        self.capacity_value = 0;
    }
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var self = Self{};

    self.allocate(allocator, 8) catch return 1;
    defer self.deallocate(allocator);

    // Access capacity to trigger the capacity function
    const cap = self.capacity();
    if (cap != 8) return 2;

    return 0;
}
