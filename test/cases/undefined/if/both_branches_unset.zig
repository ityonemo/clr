pub fn main() u8 {
    var x: u8 = 42; // start defined
    var cond: bool = undefined;
    cond = true;
    if (cond) {
        x = undefined; // set to undefined in true branch
    } else {
        x = undefined; // set to undefined in false branch
    }
    // x is undefined from both branches - should error
    return x;
}
