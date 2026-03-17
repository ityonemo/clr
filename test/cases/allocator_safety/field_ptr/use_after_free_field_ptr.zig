const std = @import("std");

const Container = struct {
    value: u8,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const container = allocator.create(Container) catch return 1;
    container.value = 42; // Initialize the value
    const field_ptr: *u8 = &container.value;
    allocator.destroy(container);
    return field_ptr.*; // Use after free!
}
