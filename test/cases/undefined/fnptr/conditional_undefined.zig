// Test: Conditional function pointer - one branch uses param (fails), other ignores it (succeeds)
fn uses_defined(_: u8) u8 {
    return 47; // Ignores parameter - no error
}

fn uses_undefined(x: u8) u8 {
    return x + 1; // Uses parameter - undefined error expected
}

pub fn main() u8 {
    var condition: bool = true;
    _ = &condition;
    var x: u8 = undefined;
    _ = &x;
    // Both branches receive undefined x, but only uses_undefined actually uses it
    const fp: *const fn (u8) u8 = if (condition) &uses_defined else &uses_undefined;
    return fp(x);
}
