// Test that fieldParentPtr on global standalone variable is detected
const Point = struct {
    x: i32,
    y: i32,
};

var global_x: i32 = 42;

fn get_x_ptr() *i32 {
    return &global_x;
}

fn get_parent(x_ptr: *i32) *Point {
    // Invalid: global_x is not a field of any struct
    return @fieldParentPtr("x", x_ptr);
}

pub fn main() u8 {
    const x_ptr = get_x_ptr();
    const parent = get_parent(x_ptr);
    return @intCast(@as(u32, @bitCast(parent.y)) & 0xFF);
}
