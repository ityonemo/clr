pub fn main() u8 {
    var x: u8 = undefined;
    var cond: bool = undefined;
    cond = true;
    if (cond) {
        x = 5;
    }
    // x may still be undefined if cond was false
    return x;
}
