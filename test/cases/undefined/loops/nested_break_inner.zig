// Break from inner loop - variable defined in inner loop before break
pub fn main() u8 {
    var x: u8 = undefined;
    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        var j: u8 = 0;
        while (j < 10) : (j += 1) {
            x = 5; // defined in inner loop
            break; // breaks inner loop only
        }
        // x is defined here (inner loop always runs at least once and defines x)
    }
    // But outer loop may not run, so x may be undefined
    return x; // ERROR: may be undefined
}
