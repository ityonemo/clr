// Inner loop breaks to outer loop - variable defined before inner break
// This tests that br_states correctly capture to ancestor loop frames
pub fn main() u8 {
    var x: u8 = undefined;
    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        x = 5; // defined in outer loop
        var j: u8 = 0;
        while (j < 10) : (j += 1) {
            if (j == 5) break; // breaks inner only, x already defined
        }
    }
    // x may be undefined if outer loop never runs
    return x; // ERROR: may be undefined
}
