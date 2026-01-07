// Tests that a stack pointer smuggled out via an array element is detected
pub fn smuggle_ptr() *u8 {
    var local: u8 = 42;
    var arr: [1]*u8 = .{&local}; // Store stack pointer in array
    _ = &arr;
    return arr[0]; // Error: smuggling stack pointer out via array
}

pub fn main() u8 {
    const ptr = smuggle_ptr();
    return ptr.*;
}
