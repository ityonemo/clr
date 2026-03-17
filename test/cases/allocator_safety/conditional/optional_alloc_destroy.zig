const std = @import("std");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    var foo: bool = true;
    _ = &foo;

    // Conditional allocation wrapped in optional
    // True branch: allocates, False branch: null
    const ptr: ?*u8 = if (foo) try alloc.create(u8) else null;

    // Destroy the allocation - should NOT report leak
    if (ptr) |p| {
        alloc.destroy(p);
    }
}
