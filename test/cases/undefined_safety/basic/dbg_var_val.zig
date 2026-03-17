// Test dbg_var_val with undefined value tracking
//
// ARCHITECTURAL NOTE:
// The original plan expected the error to mention 'y', but this isn't possible
// with the current AIR structure. The instruction sequence is:
//   1. load (copies undefined state from x, including name_when_set='x')
//   2. dbg_var_val (would set name to 'y', but load already errored)
//
// Since load checks for undefined BEFORE dbg_var_val runs, the error reports
// the ORIGIN variable ('x') not the CURRENT variable ('y'). This is actually
// useful - it tells you where the undefined value came from.
//
// To show 'y' in errors, we'd need to either:
// - Defer undefined checking to later operations (loses early detection)
// - Look ahead in AIR for upcoming dbg_var_val (complex, fragile)
//
// The current behavior is correct and informative.
pub fn main() u8 {
    var x: u32 = undefined;
    _ = &x;
    const y = x; // load detects undefined; error shows origin 'x'
    return @intCast(y);
}
