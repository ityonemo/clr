const std = @import("std");

const Iterator = struct {
    fn next(_: *Iterator) ?*u8 {
        return null;
    }
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const pointer = allocator.create(u8) catch return 1;

    var iterator = Iterator{};
    while (iterator.next()) |item| {
        allocator.destroy(item);
    }
    allocator.destroy(pointer);
    return 0;
}
