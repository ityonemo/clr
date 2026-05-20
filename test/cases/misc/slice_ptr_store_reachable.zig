// Test: allocation stored through slice ptr field is reachable after branch merge
//
// When we store an allocation into a struct's slice .ptr field via ptr_slice_ptr_ptr,
// the allocation must be reachable from the original struct after the branch merges.
// This triggers the same pattern as ArrayList.ensureTotalCapacityPrecise.

const std = @import("std");

const Container = struct {
    items: []u8,
    capacity: usize,
};

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var container: Container = .{
        .items = &.{},
        .capacity = 0,
    };

    // Allocate memory in one branch
    var condition: bool = true;
    _ = &condition; // prevent comptime eval
    if (condition) {
        const new_memory = allocator.alloc(u8, 10) catch return 1;
        // Store through ptr_slice_ptr_ptr pattern
        container.items.ptr = new_memory.ptr;
        container.items.len = new_memory.len;
        container.capacity = new_memory.len;
    }
    // At branch merge, the allocation should be reachable via container.items

    // Use and free the memory
    container.items[0] = 42;
    allocator.free(container.items);

    return 0;
}
