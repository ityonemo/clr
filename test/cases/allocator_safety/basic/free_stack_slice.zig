const std = @import("std");

pub fn main() u8 {
    var arr: [10]u8 = undefined;
    arr[0] = 42;
    const slice: []u8 = &arr;
    const allocator = std.heap.page_allocator;
    // ERROR: trying to free slice to stack array with allocator
    allocator.free(slice);
    return 0;
}
