fn uses_value(val: u8) u8 {
    return val + 1;  // Use of undefined
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;

    const callback: *const fn (u8) u8 = &uses_value;
    return callback(x);  // Passing undefined through fnptr
}
