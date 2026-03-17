noinline fn use_optional(val: ?u8) void {
    _ = val;
}

pub fn main() void {
    var x: ?u8 = undefined;
    var cond: bool = undefined;
    cond = true;
    if (cond) {
        x = 42; // one branch sets value
    } else {
        x = null; // other branch sets null
    }
    // x is defined (either value or null) - should be OK
    use_optional(x);
}
