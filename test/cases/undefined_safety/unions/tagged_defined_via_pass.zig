const Value = union(enum) {
    int: u8,
    float: f32,
};

fn defineField(v: *Value) void {
    v.* = .{ .int = 42 };
}

pub fn main() u8 {
    var v: Value = undefined;
    defineField(&v);
    return v.int; // OK: defined via callee
}
