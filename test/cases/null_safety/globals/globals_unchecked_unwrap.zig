// Test that unchecked unwrap of global null optional is detected
var global_opt: ?u8 = null;

fn unwrap_global() u8 {
    return global_opt.?;
}

pub fn main() u8 {
    // Unchecked unwrap of global null optional
    return unwrap_global();
}
