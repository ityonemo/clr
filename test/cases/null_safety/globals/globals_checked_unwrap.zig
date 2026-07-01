// Test that checked unwrap of global optional is safe
var global_opt: ?u8 = null;

fn set_global() void {
    global_opt = 42;
}

fn check_and_unwrap() u8 {
    if (global_opt) |val| {
        return val;
    }
    return 0;
}

pub fn main() u8 {
    set_global();
    return check_and_unwrap();
}
