fn escaped_ptr() *u8 {
    var foo: u8 = 0;
    foo += 1;
    return &foo;
}

pub fn main() u8 {
    const x = escaped_ptr();
    return x.*;
}
