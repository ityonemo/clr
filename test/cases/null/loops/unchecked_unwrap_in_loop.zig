// Unchecked unwrap inside loop - should error
// The optional is unwrapped without checking
pub fn main() u8 {
    var opt: ?u8 = null;
    _ = &opt;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        const val = opt.?; // Error: unwrap without check
        _ = val;
    }

    return 0;
}
