pub fn main() u8 {
    var x: u8 = undefined;
    _ = &x; // suppress unused warning
    var cond: bool = undefined;
    cond = true;
    if (cond) {
        // don't set x
    } else {
        // don't set x either
    }
    // x is still undefined - should error
    return x;
}
