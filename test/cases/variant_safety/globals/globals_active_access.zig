// Test that accessing active variant of global union is safe
const Value = union(enum) {
    int: i32,
    float: f32,
};

var global_union: Value = .{ .int = 42 };

fn set_to_int() void {
    global_union = .{ .int = 100 };
}

fn get_int() i32 {
    return global_union.int;
}

pub fn main() u8 {
    set_to_int();
    // Accessing .int when .int is active - should be fine
    const val = get_int();
    return @intCast(@as(u32, @bitCast(val)) & 0xFF);
}
