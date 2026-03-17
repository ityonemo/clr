// Test global variable name appears in error message
var global_x: u32 = undefined;

pub fn main() u8 {
    return @intCast(global_x); // Error should say "global_x"
}
