const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn escape_via_union(out: *Container) void {
    var foo: u8 = 42;
    out.* = .{ .ptr = &foo }; // escapes through union
}

pub fn main() u8 {
    var container: Container = .{ .value = 0 };
    escape_via_union(&container);
    return container.ptr.*; // use-after-return
}
