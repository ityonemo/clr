const Point = struct {
    x: u8,
    y: u8,
};

fn set_point(p: *Point) void {
    p.x = 5;
    p.y = 10;
}

pub fn main() u8 {
    var p: Point = undefined;
    set_point(&p);
    // Both fields now defined via function call
    return p.x + p.y;
}
