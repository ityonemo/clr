// Test that block bodies execute before subsequent instructions.
// This pattern occurs in Zig when using switch expressions to initialize structs.
// The switch generates blocks with bodies at high indices that must execute inline.

const std = @import("std");

// Use a regular struct, not packed, to avoid read-modify-write bit manipulation
const Mode = enum { read, write, execute };

fn getMode() Mode {
    // Use a runtime value to prevent comptime evaluation
    var x: Mode = .write;
    _ = &x;
    return x;
}

pub fn main() u8 {
    // This pattern generates AIR where:
    // 1. A value is computed via switch
    // 2. The switch generates blocks with bodies at high indices
    // 3. The block body must execute before the switch result is used
    const mode = getMode();
    const value: u8 = switch (mode) {
        .read => 10,
        .write => 20,
        .execute => 30,
    };

    // Use the value - this should NOT error because it was assigned in switch
    if (value == 20) {
        return 0; // Success
    }
    return 1;
}
