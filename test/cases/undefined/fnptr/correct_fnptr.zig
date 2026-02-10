// Test: Function pointer with correct usage - no errors expected
fn callback(x: u8) u8 {
    return x + 1;
}

pub fn main() u8 {
    var x: u8 = 42;
    _ = &x;
    const fp: *const fn (u8) u8 = &callback;
    return fp(x);
}
