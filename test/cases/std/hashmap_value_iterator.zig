const std = @import("std");

pub fn main() u8 {
    var map = std.AutoHashMap(u32, u32).init(std.heap.page_allocator);
    defer map.deinit();
    map.put(1, 2) catch return 1;

    var values = map.valueIterator();
    const value = values.next() orelse return 1;
    return @intCast(value.* - 2);
}
