const Point = struct {
    x: u8,
    y: u8,
};

fn takePoint(p: Point) u8 {
    return p.x + p.y;
}

fn getX() u8 {
    var x: u8 = 5;
    _ = &x; // prevent comptime eval
    return x;
}

fn getY() u8 {
    var y: u8 = 10;
    _ = &y; // prevent comptime eval
    return y;
}

pub fn main() u8 {
    // This should generate aggregate_init because x and y are runtime values
    const result = takePoint(.{ .x = getX(), .y = getY() });
    return result;
}
