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

    const values = [_]?*u8{ first, null, second };
    var groups = std.mem.splitScalar(?*u8, values[0..], null);
    while (groups.next()) |group| {
        for (group) |maybe_pointer| {
            const pointer = maybe_pointer orelse continue;
            allocator.destroy(pointer);
        }
    }
    return 0;
}
