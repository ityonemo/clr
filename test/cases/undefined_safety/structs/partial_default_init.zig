const Point = struct {
    x: u8 = 10, // has default value
    y: u8, // no default, will be undefined
};

pub fn main() u8 {
    var p: Point = .{ .y = undefined };
    p.y = 5;
    return p.x + p.y; // should be 15
}
