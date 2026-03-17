const std = @import("std");

fn does_free(allocator: std.mem.Allocator, ptr: *u8) void {
    allocator.destroy(ptr);  // Double free
}

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    const ptr = allocator.create(u8) catch return 1;
    allocator.destroy(ptr);

    const callback: *const fn (std.mem.Allocator, *u8) void = &does_free;
    callback(allocator, ptr);  // Passing freed pointer to be freed again

    return 0;
}
