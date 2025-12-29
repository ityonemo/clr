const Value = union(enum) {
    int: i32,
    float: f32,
};

noinline fn getIntChecked(v: *Value) i32 {
    // OK: we check the tag before accessing
    if (v.* == .int) {
        return v.int;
    }
    return 0;
}

pub fn main() u8 {
    var v: Value = .{ .int = 42 };
    const i = getIntChecked(&v);
    _ = i;
    return 0;
}

