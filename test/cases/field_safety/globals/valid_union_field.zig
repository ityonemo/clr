// Test that fieldParentPtr on global union field is valid
const Value = union {
    int: i32,
    float: f32,
};

var global_union: Value = .{ .int = 42 };

fn get_int_ptr() *i32 {
    return &global_union.int;
}

fn get_parent(int_ptr: *i32) *Value {
    return @fieldParentPtr(int_ptr, "int");
}

pub fn main() u8 {
    const int_ptr = get_int_ptr();
    const parent = get_parent(int_ptr);
    return @intCast(@as(u32, @bitCast(parent.int)) & 0xFF);
}
