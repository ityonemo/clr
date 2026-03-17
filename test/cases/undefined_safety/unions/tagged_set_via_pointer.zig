const Value = union(enum) {
    int: u8,
    float: f32,
};

fn setValue(v: *Value) void {
    v.* = .{ .int = 42 };
}

pub fn main() u8 {
    var v: Value = undefined;
    setValue(&v);
    return v.int; // OK: set via pointer
}
