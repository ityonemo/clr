const std = @import("std");

pub fn main() u8 {
    // Local GeneralPurposeAllocator - use allocator() inline
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    // Allocate with GPA - inline call to .allocator().create()
    const ptr = gpa.allocator().create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator (page_allocator)
    std.heap.page_allocator.destroy(ptr);

    return 0;
}
