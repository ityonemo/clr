// Test that storing stack pointer in global is detected as escape
var global_ptr: ?*u8 = null;

fn store_stack_ptr() void {
    var local: u8 = 42;
    global_ptr = &local;
}

fn use_global() u8 {
    return global_ptr.?.*;
}

pub fn main() u8 {
    store_stack_ptr();
    // Stack pointer escaped to global, local is gone
    return use_global();
}
