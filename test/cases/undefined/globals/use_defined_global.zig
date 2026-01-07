// Test that using a global initialized to a value is safe
// even when accessed through a function call
var global_value: u8 = 42;

fn use_global() u8 {
    return global_value;
}

pub fn main() u8 {
    // Use of defined global through function call should be fine
    return use_global();
}
