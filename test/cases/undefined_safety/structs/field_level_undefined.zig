const Point = struct {
    x: u8,
    y: u8,
};

pub fn main() u8 {
    var p: Point = .{.x = undefined, .y = undefined};
    p.x = 5;
    return p.x;
}
