fn set_value(ptr: *u8) void {
    ptr.* = 5;
}

pub fn main() u8 {
    var x: u8 = undefined;
    set_value(&x);
    return x + 1;
}
