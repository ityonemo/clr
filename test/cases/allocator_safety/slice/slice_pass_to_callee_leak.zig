const std = @import("std");
const allocator = std.heap.page_allocator;

fn consumer(slice: []u8) void {
    // Callee uses but doesn't free - causes leak
    slice[0] = 42;
}

pub fn main() u8 {
    const slice = allocator.alloc(u8, 3) catch return 1;
    consumer(slice);
    // Memory leak - callee didn't free, caller doesn't either
    return 0;
}
