const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const first = allocator.alloc(u8, 1) catch return 1;
    const second = allocator.alloc(u8, 1) catch {
        allocator.free(first);
        return 2;
    };
    first[0] = 1;
    second[0] = 2;

    var choose_first = true;
    _ = &choose_first;
    const selected = if (choose_first) first else second;
    var destination: [1]u8 = .{0};
    @memcpy(destination[0..], selected);
    _ = destination[0];

    allocator.free(first);
    allocator.free(second);
    return 0;
}
