const std = @import("std");

pub fn main() u8 {
    var arr: [10]u8 = undefined;
    arr[0] = 42;
    const allocator = std.heap.page_allocator;
    // ERROR: trying to free stack array with allocator
    allocator.free(&arr);
    return 0;
}
