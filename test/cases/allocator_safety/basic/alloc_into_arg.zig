// Test that allocations stored into argument fields don't report false leaks
const std = @import("std");

const Container = struct {
    items: ?*u8,
};

fn storeAlloc(ptr: *Container, allocator: std.mem.Allocator) !void {
    ptr.items = try allocator.create(u8);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var container = Container{ .items = null };
    try storeAlloc(&container, allocator);

    // Now container.items points to allocated memory
    // This should NOT be a leak at this point - container owns it
    // We must free it before main exits
    if (container.items) |items| {
        allocator.destroy(items);
    }
}
