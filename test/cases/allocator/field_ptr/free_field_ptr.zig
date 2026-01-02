const std = @import("std");

const Container = struct {
    value: u8,
    other: u8,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    // Use stack-allocated container (heap-allocated struct causes branch merge crash)
    var container: Container = undefined;
    container.value = 42;
    container.other = 10;

    // Take a field pointer - this should be marked with field_ptr origin
    const field_ptr: *u8 = &container.value;

    // Error: cannot free a field pointer - the memory is part of the stack container
    allocator.destroy(field_ptr);
    return 0;
}
