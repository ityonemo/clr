const Point = struct {
    x: u8,
    y: u8,
};

fn set_x_only(p: *Point) void {
    p.x = 5;
    // Forgot to set p.y
}

pub fn main() u8 {
    var p: Point = undefined;
    set_x_only(&p);
    // p.y is still undefined after call
    return p.x + p.y; // Error: p.y is undefined
}
