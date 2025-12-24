const Point = struct {
    x: u8,
    y: u8,
};

fn get_y(p: *const Point) u8 {
    return p.y; // Error: accessing undefined field through pointer
}

pub fn main() u8 {
    var p: Point = undefined;
    p.x = 5;
    // p.y is undefined
    return get_y(&p);
}
