const Container = struct {
    ptr: *u8,
};

fn escape_via_struct(out: *Container) void {
    var foo: u8 = 42;
    out.ptr = &foo; // escapes through struct field
}

pub fn main() u8 {
    var container: Container = undefined;
    escape_via_struct(&container);
    return container.ptr.*; // use-after-return
}
