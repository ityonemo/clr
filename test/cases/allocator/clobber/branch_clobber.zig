const std = @import("std");

pub fn main() void {
    var result: *u8 = undefined;
    var condition: bool = true;
    _ = &condition; // prevent comptime optimization

    if (condition) {
        result = std.heap.page_allocator.create(u8) catch unreachable;
        result = std.heap.page_allocator.create(u8) catch unreachable; // clobber! first allocation leaked
    } else {
        result = std.heap.page_allocator.create(u8) catch unreachable;
    }
    std.heap.page_allocator.destroy(result);
}
