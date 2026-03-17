// Tests that a pointer to a value parameter escapes via array
noinline fn escaped_param_ptr(param: u8) *const u8 {
    var arr: [1]*const u8 = .{&param}; // Store pointer to parameter in array
    _ = &arr;
    return arr[0]; // Error: parameter pointer escapes via array
}

pub fn main() u8 {
    const ptr = escaped_param_ptr(42);
    return ptr.*;
}
