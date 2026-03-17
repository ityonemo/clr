// Variable defined before loop, used after the loop
pub fn main() u8 {
    var x: u8 = 5; // defined before loop
    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        x += 1;
    }
    return x; // should be defined, no error
}
