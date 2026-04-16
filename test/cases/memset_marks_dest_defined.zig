// Test that @memset marks destination elements as defined.
// After setting with a value, the destination should be defined.

pub fn main() u8 {
    var dest: [4]u8 = undefined;

    // After memset, dest should be defined
    @memset(&dest, 42);

    // This should NOT trigger "use of undefined value"
    return dest[0];
}
