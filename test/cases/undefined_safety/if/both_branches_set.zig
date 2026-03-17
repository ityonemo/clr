pub fn main() u8 {
    var x: u8 = undefined;
    var cond: bool = undefined;
    cond = true;
    if (cond) {
        x = 5;
    } else {
        x = 10;
    }
    // x is defined regardless of which branch executes
    return x;
}
