// Variable defined before nested loops, used after
pub fn main() u8 {
    var x: u8 = 10; // defined before loops
    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            x += 1;
        }
    }
    return x; // OK: x was defined before loops
}
