const std = @import("std");

pub fn main() u8 {
    var map = std.AutoHashMap(u8, *u8).init(std.heap.page_allocator);
    defer map.deinit();

    var values = map.valueIterator();
    while (values.next()) |value| {
        std.heap.page_allocator.destroy(value.*);
    }

    return 0;
}
