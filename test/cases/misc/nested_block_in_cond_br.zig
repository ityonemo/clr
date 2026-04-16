// Test that nested blocks inside cond_br branches have their bodies properly expanded.
// This exercises the case where a safety check block inside a conditional branch
// contains a cond_br that narrows null_safety state.
//
// The pattern is:
// 1. Enter a cond_br branch (creates sub-function)
// 2. Inside the branch, check an optional (is_non_null)
// 3. A safety block with nested cond_br narrows the null state
// 4. Unwrap the optional (optional_payload)
//
// Without proper block expansion in sub-functions, the narrowing cond_br
// is missing, causing false positive "unchecked optional unwrap" errors.

fn getOptional(cond: bool) ?u8 {
    if (cond) {
        return 42;
    }
    return null;
}

fn process(val: u8) u8 {
    return val +% 1;
}

pub fn main() u8 {
    var cond: bool = true;
    _ = &cond;

    // Outer conditional - creates sub-function for true branch
    if (cond) {
        const maybe_val = getOptional(cond);

        // Check optional - generates is_non_null + safety block with cond_br
        if (maybe_val) |val| {
            // Unwrap - generates optional_payload
            // This should NOT trigger false positive because the safety
            // block's cond_br narrowed the null state
            return process(val);
        }
    }

    return 0;
}
