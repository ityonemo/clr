const std = @import("std");

const MyArray = struct {
    items: [*]u8,
    capacity: usize,
    allocator: std.mem.Allocator,

    pub fn resize(self: *MyArray, new_cap: usize) !void {
        const new_memory = try self.allocator.alloc(u8, new_cap);
        // Store the .ptr into the struct - this should transfer ownership
        self.items = new_memory.ptr;
        self.capacity = new_memory.len;
    }
};

pub fn main() u8 {
    var arr = MyArray{
        .items = undefined,
        .capacity = 0,
        .allocator = std.heap.page_allocator,
    };
    arr.resize(10) catch return 1;
    // Caller should free via arr.allocator.free(arr.items[0..arr.capacity])
    arr.allocator.free(arr.items[0..arr.capacity]);
    return 0;
}
