const Value = union(enum) {
    int: u8,
    float: f32,
};

fn useValue(v: Value) u8 {
    return v.int; // Error: field is undefined
}

pub fn main() u8 {
    var v: Value = undefined;
    v = .{ .int = undefined };
    return useValue(v);
}
