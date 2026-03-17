const std = @import("std");

const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

pub fn main() !void {
    var container: Container = .{
        .ptr = try std.heap.page_allocator.create(u8),
    };
    container = .{ .value = 42 }; // variant change! allocation leaked
}
