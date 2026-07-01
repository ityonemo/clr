fn no_escape(ptr: *u8) *u8 {
    ptr.* += 1;
    return ptr;
}

pub fn main() u8 {
    var x: u8 = 10;
    const result = no_escape(&x);
    return result.*;
}
