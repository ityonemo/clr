// Test that allocations stored into slice fields via ptr_slice_ptr_ptr are tracked correctly
const std = @import("std");

const Container = struct {
    items: []u8,
};

fn updateSlice(self: *Container, allocator: std.mem.Allocator, new_capacity: usize) !void {
    // Allocate new memory
    const new_mem = try allocator.alloc(u8, new_capacity);

    // Store new pointer into self.items.ptr (via ptr_slice_ptr_ptr pattern)
    self.items.ptr = new_mem.ptr;
    self.items.len = new_mem.len;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var container = Container{ .items = &.{} };
    try updateSlice(&container, allocator, 10);

    // container.items now points to allocated memory
    // This should NOT be a leak at function end - container owns it
    // We must free it before main exits
    allocator.free(container.items);
}
