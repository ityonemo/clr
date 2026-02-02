// Test that accessing an undefined field in a mixed struct init is detected
const Point = struct {
    x: i32,
    y: i32,
};

var global_point: Point = .{ .x = 42, .y = undefined };

fn use_struct() i32 {
    return global_point.y;  // Should error - y is undefined
}

pub fn main() u8 {
    const result = use_struct();
    _ = result;
    return 0;
}
