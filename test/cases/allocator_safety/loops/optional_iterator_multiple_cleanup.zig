const std = @import("std");

const Iterator = struct {
    first: *u8,
    second: *u8,
    index: u2 = 0,

    fn next(self: *Iterator) ?*u8 {
        defer self.index += 1;
        return switch (self.index) {
            0 => self.first,
            1 => self.second,
            else => null,
        };
    }
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const first = allocator.create(u8) catch return 1;
    const second = allocator.create(u8) catch {
        allocator.destroy(first);
        return 2;
    };

    var iterator = Iterator{ .first = first, .second = second };
    while (iterator.next()) |pointer| {
        allocator.destroy(pointer);
    }
    return 0;
}
