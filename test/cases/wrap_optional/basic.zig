// Test that wrap_optional correctly initializes memory_safety on optionals.
// This triggers the wrap_optional AIR instruction.
pub fn main() u8 {
    var x: u8 = 42;
    const ptr: *u8 = &x;

    // Wrapping a value in an optional triggers wrap_optional
    const opt: ?*u8 = ptr;
    _ = opt;

    return 0;
}
