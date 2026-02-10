// Test: Switch-based function selection - only one branch uses the parameter
const Op = enum { add, sub, mul };

fn add_fn(x: u8) u8 {
    return x + 1; // Uses parameter - undefined error expected
}

fn sub_fn(_: u8) u8 {
    return 99; // Ignores parameter - no error
}

fn mul_fn(_: u8) u8 {
    return 77; // Ignores parameter - no error
}

fn get_fn(op: Op) *const fn (u8) u8 {
    return switch (op) {
        .add => &add_fn,
        .sub => &sub_fn,
        .mul => &mul_fn,
    };
}

pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x;
    var op: Op = .add;
    _ = &op;
    const fp = get_fn(op);
    return fp(x); // Only add_fn branch will fail
}
