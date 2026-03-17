const Point = struct {
    x: u8,
    y: u8,
};

fn use_point(p: Point) u8 {
    return p.x + p.y;
}

pub fn main() u8 {
    var p: Point = undefined;
    p.x = 5;
    // p.y is undefined, passing struct with undefined field
    return use_point(p); // Error: p.y is undefined when passed
}
