// Test that @memcpy marks destination elements as defined.
// After copying from a defined source, the destination should be defined.

pub fn main() u8 {
    var src: [4]u8 = .{ 1, 2, 3, 4 };
    var dest: [4]u8 = undefined;

    // After memcpy, dest should be defined
    @memcpy(&dest, &src);

    // This should NOT trigger "use of undefined value"
    return dest[0];
}
