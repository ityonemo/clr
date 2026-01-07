// Test that global optional assigned non-null can be safely unwrapped
var global_opt: ?u8 = null;

fn set_global() void {
    global_opt = 42;
}

fn unwrap_global() u8 {
    return global_opt.?;
}

pub fn main() u8 {
    set_global();
    // After assignment, unwrap should be safe
    return unwrap_global();
}
