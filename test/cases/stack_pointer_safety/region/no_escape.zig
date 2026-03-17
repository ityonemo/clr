// Tests that passed-in pointers stored in arrays do not trigger false positives
pub fn identity(ptr: *u8) *u8 {
    var arr: [1]*u8 = .{ptr}; // Store passed-in pointer in array
    _ = &arr;
    return arr[0]; // OK: returning pointer that came from caller
}

pub fn main() u8 {
    var x: u8 = 42;
    const ptr = identity(&x);
    return ptr.*;
}
