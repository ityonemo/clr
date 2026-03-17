const Value = union(enum) {
    int: u8,
    float: f32,
};

fn missField(v: *Value) void {
    // Doesn't set the field
    _ = v;
}

pub fn main() u8 {
    var v: Value = undefined;
    v = .{ .int = undefined };
    missField(&v);
    return v.int; // Error: field still undefined
}
