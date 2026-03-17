// Null check inside loop - should pass
// The optional is checked before unwrap in each iteration
pub fn main() u8 {
    var opt: ?u8 = 5;
    _ = &opt;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        if (opt) |val| {
            _ = val;
        }
    }

    return 0;
}
