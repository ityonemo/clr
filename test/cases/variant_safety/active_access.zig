const Value = union(enum) {
    int: i32,
    float: f32,
};

noinline fn getInt(v: *Value) i32 {
    // OK: accessing .int when .int is active
    return v.int;
}

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    const i = getInt(&v);
    _ = i;
    return 0;
}

