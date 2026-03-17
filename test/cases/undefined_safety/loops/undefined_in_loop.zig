// Undefined variable used inside loop body
pub fn main() u8 {
    var x: u8 = undefined; // undefined
    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        x += 1; // ERROR: use of undefined value
    }
    return x;
}
