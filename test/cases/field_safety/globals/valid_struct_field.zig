// Test that fieldParentPtr on global struct field is valid
const Point = struct {
    x: i32,
    y: i32,
};

var global_point: Point = .{ .x = 10, .y = 20 };

fn get_x_ptr() *i32 {
    return &global_point.x;
}

fn get_parent(x_ptr: *i32) *Point {
    return @fieldParentPtr(x_ptr, "x");
}

pub fn main() u8 {
    const x_ptr = get_x_ptr();
    const parent = get_parent(x_ptr);
    return @intCast(@as(u32, @bitCast(parent.y)) & 0xFF);
}
