// Test that switch on global union variant allows safe field access
const Value = union(enum) {
    int: i32,
    float: f32,
    boolean: bool,
};

var global_union: Value = .{ .int = 42 };

fn set_to_float() void {
    global_union = .{ .float = 3.14 };
}

fn get_with_switch() i32 {
    return switch (global_union) {
        .int => |i| i,
        .float => 0,
        .boolean => 1,
    };
}

pub fn main() u8 {
    set_to_float();
    // Switch should safely handle variant dispatch
    const result = get_with_switch();
    _ = result;
    return 0;
}
