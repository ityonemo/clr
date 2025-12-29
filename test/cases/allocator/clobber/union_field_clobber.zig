const std = @import("std");

const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

pub fn main() !void {
    var container: Container = .{
        .ptr = try std.heap.page_allocator.create(u8),
    };
    container.ptr = try std.heap.page_allocator.create(u8); // clobber! first allocation leaked
    std.heap.page_allocator.destroy(container.ptr);
}
