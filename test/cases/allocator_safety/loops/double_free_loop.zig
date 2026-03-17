// Double free in loop - free same pointer multiple times
// The analysis correctly detects this as double-free:
// 1. Loop iteration 1: ptr freed
// 2. Loop iteration 2: ptr already freed -> double-free detected
// Exit paths (br to loop exit) are captured separately and don't interfere with iteration state.
const std = @import("std");

pub fn main() u8 {
    const allocator = std.heap.page_allocator;

    const ptr = allocator.create(u8) catch return 1;
    ptr.* = 42;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        allocator.destroy(ptr); // Would be double free on iterations 2+
    }

    return 0;
}
