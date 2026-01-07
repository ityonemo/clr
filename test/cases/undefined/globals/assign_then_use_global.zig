// Test that assigning to an undefined global then using it is safe
var global_value: u8 = undefined;

pub fn main() u8 {
    global_value = 42;
    // After assignment, global should be defined
    return global_value;
}
