const std = @import("std");

/// Struct with an array field - common pattern that triggers struct_field_val
/// with a region refinement type.
const WyhashState = struct {
    a: u64,
    b: u64,
    buf: [32]u8,  // This is the region field
    buf_len: u8,

    pub fn init(seed: u64) WyhashState {
        return WyhashState{
            .a = seed,
            .b = seed,
            .buf = undefined,
            .buf_len = 0,
        };
    }
};

pub fn main() u8 {
    const state = WyhashState.init(0);
    // Access the array field - this generates struct_field_val with region type
    _ = state.buf;
    return 0;
}
