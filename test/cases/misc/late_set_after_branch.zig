// Test: Late set after branch makes value consistent
//
// callee() takes a pointer and has two return paths:
// - True branch: returns early WITHOUT setting *ptr
// - False branch: continues after if, sets *ptr, then returns
//
// Key insight: The true branch returns early, so *ptr is still
// undefined when that return happens. But the false path continues
// AFTER the if statement and sets *ptr before returning.
//
// At cond_br merge time:
// - True branch: returned with *ptr undefined
// - False branch: hasn't returned yet, *ptr still undefined
//
// After cond_br, the false path continues:
// - ptr.* = 10; (now defined)
// - return; (with *ptr defined)
//
// The issue: when merging return states, we need to capture
// the state AT THE TIME OF EACH RETURN, not at cond_br time.
//
// Expected: ERROR - "conflicting" state detected
// TODO: Flesh out the error message to be more descriptive

fn get_bool() bool {
    return true;
}

fn callee(ptr: *u8) void {
    if (get_bool()) {
        // Early return WITHOUT setting the value
        return;
    }

    // False path: set the value AFTER the if
    ptr.* = 10;
    return;
}

pub fn main() u8 {
    var x: u8 = undefined;
    callee(&x);
    return x;  // Should error - x might be undefined (true branch didn't set it)
}
