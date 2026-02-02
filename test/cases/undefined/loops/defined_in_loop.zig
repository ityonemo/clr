// Variable only defined in loop body, used after loop
// The loop may not execute (condition could be false), so using
// the variable after the loop should be inconsistent
pub fn main() u8 {
    var x: u8 = undefined;
    var i: u8 = 0;
    while (i < 10) : (i += 1) {
        x = 5; // defined only if loop executes
    }
    return x; // ERROR: inconsistent - may be undefined if loop doesn't run
}
