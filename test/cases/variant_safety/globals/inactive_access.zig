// Test that accessing inactive variant of global union is detected
const Value = union(enum) {
    int: i32,
    float: f32,
};

var global_union: Value = .{ .int = 42 };

fn set_to_float() void {
    global_union = .{ .float = 3.14 };
}

fn get_int() i32 {
    return global_union.int;
}

pub fn main() u8 {
    set_to_float();
    // Accessing .int when .float is active
    const val = get_int();
    return @intCast(@as(u32, @bitCast(val)) & 0xFF);
}
