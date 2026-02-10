// Test optional_payload_ptr tracking through pointer modification
pub fn main() u8 {
    var opt: ?u32 = 42;
    const ptr: *u32 = &(opt.?); // optional_payload_ptr
    ptr.* = 10;
    return @intCast(opt.?);
}
