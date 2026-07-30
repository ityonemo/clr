const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const first = allocator.create(u8) catch return 1;
    const second = allocator.create(u8) catch {
        allocator.destroy(first);
        return 2;
    };
    first.* = 1;
    second.* = 2;

    var map = std.AutoHashMap(u8, *u8).init(allocator);
    map.put(1, first) catch {
        allocator.destroy(first);
        allocator.destroy(second);
        map.deinit();
        return 3;
    };
    map.put(2, second) catch {
        allocator.destroy(first);
        allocator.destroy(second);
        map.deinit();
        return 4;
    };

    var values = map.valueIterator();
    while (values.next()) |value| {
        allocator.destroy(value.*);
    }
    map.deinit();
    return 0;
}
