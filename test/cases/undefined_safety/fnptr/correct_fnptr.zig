fn add_one(val: u8) u8 {
    return val + 1;
}

pub fn main() u8 {
    const x: u8 = 5;  // Defined value

    const callback: *const fn (u8) u8 = &add_one;
    return callback(x);  // Correct usage - defined value
}
