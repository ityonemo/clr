fn uses_value(val: u8) u8 {
    return val + 1;  // Use of undefined in this branch
}

fn ignores_value(val: u8) u8 {
    _ = val;
    return 42;  // Doesn't use val
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;

    const cond = true;
    const callback: *const fn (u8) u8 = if (cond) &uses_value else &ignores_value;
    return callback(x);  // One possible branch uses undefined
}
