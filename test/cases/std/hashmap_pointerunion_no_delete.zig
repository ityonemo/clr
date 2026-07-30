const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    var stack_value: u8 = 1;
    const heap_value = allocator.create(u8) catch return 1;
    heap_value.* = 2;

    var map = std.AutoHashMap(u8, *u8).init(allocator);
    map.put(1, &stack_value) catch {
        allocator.destroy(heap_value);
        map.deinit();
        return 2;
    };
    map.put(2, heap_value) catch {
        allocator.destroy(heap_value);
        map.deinit();
        return 3;
    };

    allocator.destroy(heap_value);
    map.deinit();
    return 0;
}
