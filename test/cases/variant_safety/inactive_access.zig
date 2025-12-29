const Value = union(enum) {
    int: i32,
    float: f32,
};

noinline fn getFloat(v: *Value) f32 {
    // Error: accessing .float when .int is active
    return v.float;
}

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    const f = getFloat(&v);
    _ = f;
    return 0;
}

