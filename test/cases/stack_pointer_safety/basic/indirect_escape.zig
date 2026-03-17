fn indirect_escape() *u8 {
    var foo: u8 = 42;
    const ptr = &foo;
    return ptr;
}

pub fn main() u8 {
    const x = indirect_escape();
    return x.*;
}
