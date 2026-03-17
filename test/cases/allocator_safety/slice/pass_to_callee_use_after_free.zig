const std = @import("std");
const allocator = std.heap.page_allocator;

fn consumer(slice: []u8) void {
    slice[0] = 42;
    allocator.free(slice);
}

pub fn main() u8 {
    const slice = allocator.alloc(u8, 3) catch return 1;
    consumer(slice);
    // Use after free - callee already freed
    return slice[0];
}
