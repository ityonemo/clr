// Test case: freeing via pointer arithmetic
// This pattern is used by HashMap - allocate a block, store a derived pointer,
// then recover the original pointer via reverse arithmetic for freeing.

const std = @import("std");

const Header = struct {
    capacity: usize,
};

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    // Allocate a block: [Header][Data...]
    const total_size = @sizeOf(Header) + 16;
    const slice = allocator.alloc(u8, total_size) catch return 1;
    const ptr: [*]u8 = slice.ptr;

    // Store derived pointer (skip header)
    const data_ptr = ptr + @sizeOf(Header);

    // Write some data
    data_ptr[0] = 42;

    // Recover original pointer via reverse arithmetic (like HashMap.header())
    const recovered_ptr = data_ptr - @sizeOf(Header);

    // Free via the recovered pointer
    const free_slice = recovered_ptr[0..total_size];
    allocator.free(free_slice);

    return 0;
}
