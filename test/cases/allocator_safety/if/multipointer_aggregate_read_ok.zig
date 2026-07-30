const std = @import("std");

const Item = struct { value: u8 };

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const first = allocator.create(Item) catch return 1;
    const second = allocator.create(Item) catch {
        allocator.destroy(first);
        return 2;
    };
    first.* = .{ .value = 1 };
    second.* = .{ .value = 2 };

    var choose_first = true;
    _ = &choose_first;
    const selected = if (choose_first) first else second;
    _ = selected.value;

    allocator.destroy(first);
    allocator.destroy(second);
    return 0;
}
