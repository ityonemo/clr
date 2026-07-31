const std = @import("std");

const Iterator = struct {
    items: []const ?*u8,
    yielded: bool = false,

    fn next(self: *Iterator) ?[]const ?*u8 {
        if (self.yielded) return null;
        self.yielded = true;
        return self.items;
    }
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const first = allocator.create(u8) catch return 1;
    const second = allocator.create(u8) catch {
        allocator.destroy(first);
        return 2;
    };
    const items = [_]?*u8{ first, null, second };

    var iterator = Iterator{ .items = items[0..] };
    while (iterator.next()) |group| {
        for (group) |maybe_pointer| {
            const pointer = maybe_pointer orelse continue;
            allocator.destroy(pointer);
        }
    }
    return 0;
}
