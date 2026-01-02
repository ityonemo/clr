const std = @import("std");

const Container = struct {
    ptr: *u8,
    other: u8,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Use stack-allocated container to avoid branch merge bug with heap structs
    var container: Container = undefined;
    container.ptr = allocator.create(u8) catch return 1;
    container.ptr.* = 42;
    container.other = 10;

    // Take a field pointer (this tests that struct_field_ptr doesn't break things)
    const field_ptr = &container.other;
    _ = field_ptr.*;

    // OK: freeing the heap allocation through the struct field, not the field pointer
    allocator.destroy(container.ptr);
    return 0;
}
