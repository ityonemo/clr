const Value = union(enum) {
    int: i32,
    float: f32,
};

noinline fn getFloatPtr(v: *Value) *f32 {
    // Error: accessing .float field pointer when .int is active
    return &v.float;
}

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    const f = getFloatPtr(&v);
    _ = f;
    return 0;
}
