// Test that using a global initialized to a value is safe
var global_value: u8 = 42;

pub fn main() u8 {
    // Use of defined global should be fine
    return global_value;
}
