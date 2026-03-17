// Test that using a global initialized to undefined is detected
// even when accessed through a function call
var global_value: u8 = undefined;

fn use_global() u8 {
    return global_value;
}

pub fn main() u8 {
    // Use of undefined global through function call should be caught
    return use_global();
}
