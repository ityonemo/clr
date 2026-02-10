// Test: Function pointer callback receives undefined value
fn callback(x: u8) u8 {
    return x + 1; // Use of undefined 'x' should be detected
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;
    const fp: *const fn (u8) u8 = &callback;
    return fp(x);
}
