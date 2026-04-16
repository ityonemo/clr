fn nested(a: bool, b: bool) u8 {
    var x: u8 = undefined;
    if (a) {
        if (b) {
            x = 1;
        } else {
            x = 2;
        }
    } else {
        x = 3;
    }
    return x;
}

pub fn main() u8 {
    // All branches set x, so no undefined error
    _ = nested(true, true);
    _ = nested(true, false);
    _ = nested(false, true);
    return 0;
}
