const std = @import("std");

var global_value: u8 = 42;

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    // Attempting to free a pointer to a global variable should be an error
    allocator.destroy(&global_value);
    return 0;
}
