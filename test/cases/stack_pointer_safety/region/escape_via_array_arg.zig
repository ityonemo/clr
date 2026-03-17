// Tests that a stack pointer escaping via an array argument is detected
fn store_in_out(out: *[1]*u8, ptr: *u8) void {
    out[0] = ptr;
}

pub fn escape_via_arg() *u8 {
    var x: u8 = 42;
    var arr: [1]*u8 = undefined;
    _ = &arr;
    store_in_out(&arr, &x); // Stack pointer escapes via argument
    return arr[0]; // Error: returning stack pointer that escaped via arg
}

pub fn main() u8 {
    const ptr = escape_via_arg();
    return ptr.*;
}
