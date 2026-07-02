const std = @import("std");

const Allocation = struct {
    allocator: std.mem.Allocator,
    ptr: *u8,
};

fn allocate(allocator: std.mem.Allocator) !Allocation {
    return .{ .allocator = allocator, .ptr = try allocator.create(u8) };
}

pub fn main() void {
    const allocation = allocate(std.heap.page_allocator) catch return;
    allocation.allocator.destroy(allocation.ptr);
}
