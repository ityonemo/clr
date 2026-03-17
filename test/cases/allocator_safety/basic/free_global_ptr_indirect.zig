const std = @import("std");

var global_value: u8 = 42;
const allocator = std.heap.page_allocator;

fn do_free(ptr: *u8) void {
    allocator.destroy(ptr);
}

pub fn main() u8 {
    // Pass pointer to global through a function
    do_free(&global_value);
    return 0;
}
