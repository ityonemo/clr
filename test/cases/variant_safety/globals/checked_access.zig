// Test that checked access of global union variant is safe
const Value = union(enum) {
    int: i32,
    float: f32,
};

var global_union: Value = .{ .int = 42 };

fn set_to_float() void {
    global_union = .{ .float = 3.14 };
}

fn get_checked() u8 {
    if (global_union == .int) {
        return @intCast(@as(u32, @bitCast(global_union.int)) & 0xFF);
    }
    return 0;
}

pub fn main() u8 {
    set_to_float();
    // Checked access - should be safe (will return 0)
    return get_checked();
}
