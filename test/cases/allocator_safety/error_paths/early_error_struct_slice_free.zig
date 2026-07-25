const std = @import("std");

const Item = struct {
    value: usize,
};

const Buffer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    items: []*Item,
    head: usize = 0,
    tail: usize = 0,
    len: usize = 0,

    pub fn init(allocator: std.mem.Allocator, len: usize) !Buffer {
        if (len == 0) return error.InvalidLength;

        return Self{
            .allocator = allocator,
            .items = try allocator.alloc(*Item, len),
        };
    }

    fn deinit(self: *Buffer) void {
        self.allocator.free(self.items);
        self.* = undefined;
    }
};

pub fn main() !void {
    var map = std.AutoHashMap(usize, Buffer).init(std.heap.page_allocator);
    defer map.deinit();

    try map.put(1, try Buffer.init(std.heap.page_allocator, 1));

    var iterator = map.iterator();
    while (iterator.next()) |entry| {
        entry.value_ptr.deinit();
        break;
    }
}
