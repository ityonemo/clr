// Test case: freeing via pointer arithmetic with optional pointer
// HashMap stores metadata as ?[*]Metadata, and header() does:
//   @ptrCast(@as([*]Header, @ptrCast(@alignCast(self.metadata.?))) - 1)

const std = @import("std");

const Header = struct {
    capacity: usize,
};

const Metadata = packed struct {
    fingerprint: u7 = 0,
    used: bool = false,
};

const Self = struct {
    metadata: ?[*]Metadata = null,

    fn header(self: Self) *Header {
        // This is the HashMap pattern: unwrap optional, cast, subtract
        return @ptrCast(@as([*]Header, @ptrCast(@alignCast(self.metadata.?))) - 1);
    }

    fn capacity(self: Self) usize {
        if (self.metadata == null) return 0;
        return self.header().capacity;
    }

    fn allocate(self: *Self, allocator: std.mem.Allocator, new_capacity: usize) !void {
        const total_size = @sizeOf(Header) + new_capacity * @sizeOf(Metadata);
        const slice = try allocator.alloc(u8, total_size);
        const ptr: [*]u8 = slice.ptr;

        const hdr: *Header = @ptrCast(@alignCast(ptr));
        hdr.capacity = new_capacity;

        const metadata_ptr = ptr + @sizeOf(Header);
        self.metadata = @ptrCast(@alignCast(metadata_ptr));
    }

    fn deallocate(self: *Self, allocator: std.mem.Allocator) void {
        if (self.metadata == null) return;

        const cap = self.capacity();
        const total_size = @sizeOf(Header) + cap * @sizeOf(Metadata);

        const slice = @as([*]u8, @ptrCast(@alignCast(self.header())))[0..total_size];
        allocator.free(slice);

        self.metadata = null;
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
