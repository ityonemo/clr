const std = @import("std");

// Global GeneralPurposeAllocator
var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = gpa.allocator();

pub fn main() u8 {
    // Allocate with global GPA
    const ptr = alloc.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator (page_allocator)
    std.heap.page_allocator.destroy(ptr);

    return 0;
}
