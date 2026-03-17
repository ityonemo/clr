// Test that return values from both branches are properly merged.
// True branch returns defined, false branch returns undefined.
// The result should be "potentially undefined" since either branch could execute.
// BUG: Currently, only the first branch's return is kept, so we incorrectly get "defined".

fn returns_from_both_branches(cond: bool) u8 {
    var x: u8 = undefined;
    _ = &x; // suppress unused warning
    if (cond) {
        return 42; // true branch: returns defined (runs first in cond_br)
    } else {
        return x; // false branch: returns undefined (would trigger error, but ret_safe is reached first in this order)
    }
}

pub fn main() u8 {
    const cond: bool = true; // comptime known, but CLR doesn't track
    const result = returns_from_both_branches(cond);
    return result; // BUG: currently passes because only true branch's return (defined) is used
}
