// Test that storing parameter pointer in global is detected as escape
var global_ptr: ?*u8 = null;

fn store_param_ptr(ptr: *u8) void {
    global_ptr = ptr;
}

fn use_global() u8 {
    return global_ptr.?.*;
}

fn caller() void {
    var local: u8 = 42;
    store_param_ptr(&local);
}

pub fn main() u8 {
    caller();
    // Parameter pointer escaped to global, caller's local is gone
    return use_global();
}
