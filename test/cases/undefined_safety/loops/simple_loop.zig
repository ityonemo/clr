// Simplest possible loop - just iterates and returns
pub fn main() u8 {
    var i: u8 = 0;
    while (i < 10) : (i += 1) {}
    return i;
}
