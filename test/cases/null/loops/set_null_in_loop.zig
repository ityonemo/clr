// Set null in loop then unwrap after - may be null
// The optional may have been set to null during loop
pub fn main() u8 {
    var opt: ?u8 = 5;

    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        if (i == 2) {
            opt = null;
        }
    }

    const val = opt.?; // Error: may be null from loop
    return val;
}
