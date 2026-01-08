const std = @import("std");

var global_value: u8 = 42;
const allocator = std.heap.page_allocator;

// noinline prevents Zig from optimizing away the indirection
noinline fn identity(ptr: *u8) *u8 {
    return ptr;
}

fn do_free(ptr: *u8) void {
    allocator.destroy(ptr);
}

pub fn main() u8 {
    // Pass global pointer through identity to prevent mainlining
    do_free(identity(&global_value));
    return 0;
}
