// Variable defined before break, loop breaks early
// Without value tracking, analysis doesn't know that i=0 < 10,
// so it considers both "loop runs" and "loop doesn't run" paths.
// This produces inconsistent state - conservative but correct.
pub fn main() u8 {
    var x: u8 = undefined;
    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        x = 5; // defined in "loop runs" path
        break; // loop exits
    }
    // x may be undefined if loop condition was false initially
    return x; // ERROR: inconsistent - may be undefined
}
