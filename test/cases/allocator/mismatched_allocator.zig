const std = @import("std");

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa_allocator = gpa.allocator();
    const page_allocator = std.heap.page_allocator;

    // Allocate with page_allocator
    const ptr = page_allocator.create(u8) catch return 1;
    ptr.* = 42;

    // ERROR: Destroy with different allocator
    gpa_allocator.destroy(ptr);

    return 0;
}
