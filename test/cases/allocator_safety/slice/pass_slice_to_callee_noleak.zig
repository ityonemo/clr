const std = @import("std");
const allocator = std.heap.page_allocator;

fn consumer(slice: []u8) void {
    // Callee receives slice and frees it - no leak
    slice[0] = 42;
    allocator.free(slice);
}

pub fn main() u8 {
    const slice = allocator.alloc(u8, 3) catch return 1;
    consumer(slice);
    // Callee freed it - no leak
    return 0;
}
