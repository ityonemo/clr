// Tests that a stack pointer accessed through an array element is detected when returned
fn helper(arr: *[1]*u8) *u8 {
    return arr[0]; // Returns stack pointer via array element
}

pub fn escaped_via_element() *u8 {
    var x: u8 = 42;
    var arr: [1]*u8 = .{&x}; // Store stack pointer in array
    _ = &arr;
    return helper(&arr); // Error: stack pointer escapes via array element
}

pub fn main() u8 {
    const ptr = escaped_via_element();
    return ptr.*;
}
