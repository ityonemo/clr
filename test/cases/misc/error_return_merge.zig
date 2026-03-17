// Test merging error unions where one branch returns an error
// This exercises the case where WrapErrunionErr creates an .unimplemented payload
// that must be handled during branch merging

const std = @import("std");

fn mayFail(should_fail: bool) !u32 {
    if (should_fail) {
        return error.SomethingWentWrong;
    }
    return 42;
}

pub fn main() u8 {
    // This creates branches where one has error (unimplemented payload)
    // and one has success (valid payload)
    const result = mayFail(false) catch {
        return 1;
    };
    if (result != 42) return 2;
    return 0;
}
