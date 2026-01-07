// Test that assigning to an undefined global in one function
// and using it in another is safe
var global_value: u8 = undefined;

fn assign_global() void {
    global_value = 42;
}

fn use_global() u8 {
    return global_value;
}

pub fn main() u8 {
    assign_global();
    return use_global();
}
