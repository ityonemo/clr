fn escape_via_arg(out: **u8) void {
    var foo: u8 = 42;
    out.* = &foo; // escapes through pointer arg
}

pub fn main() u8 {
    var result: *u8 = undefined;
    escape_via_arg(&result);
    return result.*; // use-after-return
}
