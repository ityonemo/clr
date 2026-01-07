// Test that using a global initialized to undefined is detected
var global_value: u8 = undefined;

pub fn main() u8 {
    // Use of undefined global should be caught
    return global_value;
}
