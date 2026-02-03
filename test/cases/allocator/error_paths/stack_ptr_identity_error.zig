// Test: Stack pointer passed through identity function that errors on poison value
// The stack pointer should remain valid on both success and error paths.
const std = @import("std");

fn identityOrError(x: *u8) !*u8 {
    if (x.* == 0xFF) return error.PoisonValue;
    return x;
}

pub fn main() u8 {
    var stack_val: u8 = 42;
    const ptr: *u8 = &stack_val;

    // Pass stack pointer through function that may error
    const result = identityOrError(ptr) catch {
        // On error path, ptr still points to valid stack memory
        ptr.* = 0; // Should be safe to use
        return 1;
    };

    // On success path, result points to same stack memory
    result.* = 100;
    return 0;
}
