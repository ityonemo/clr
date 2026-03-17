const std = @import("std");

fn allocate_with(alloc: std.mem.Allocator) *u8 {
    return alloc.create(u8) catch @panic("allocation failed");
}

pub fn main() u8 {
    // Create local GPA and pass its allocator to a function
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const alloc = gpa.allocator();

    const ptr = allocate_with(alloc);
    ptr.* = 42;

    // ERROR: Destroy with different allocator (page_allocator)
    std.heap.page_allocator.destroy(ptr);

    return 0;
}
