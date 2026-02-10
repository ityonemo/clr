fn add_fn(val: u8) u8 {
    return val + 1;  // Uses undefined
}

fn sub_fn(val: u8) u8 {
    _ = val;
    return 10;  // Doesn't use val
}

fn mul_fn(val: u8) u8 {
    _ = val;
    return 20;  // Doesn't use val
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;

    const op: u8 = 0;
    const callback: *const fn (u8) u8 = switch (op) {
        0 => &add_fn,
        1 => &sub_fn,
        else => &mul_fn,
    };
    return callback(x);  // One switch branch uses undefined
}
