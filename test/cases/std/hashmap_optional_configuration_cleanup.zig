const std = @import("std");

noinline fn configured() bool {
    return false;
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var map = std.AutoHashMap(u8, *u8).init(allocator);
    defer map.deinit();

    if (configured()) {
        const pointer = allocator.create(u8) catch return 1;
        map.put(1, pointer) catch {
            allocator.destroy(pointer);
            return 2;
        };
    }

    var values = map.valueIterator();
    while (values.next()) |pointer| {
        allocator.destroy(pointer.*);
    }
    return 0;
}
