// Test: Allocation in a loop - error path should clear phantom allocation
// Each iteration allocates, uses, and frees - should pass with no leaks
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;
    
    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const ptr = allocator.create(u8) catch return 1;
        ptr.* = i;
        allocator.destroy(ptr);
    }
    return 0;
}
