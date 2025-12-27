// Top-level const with partial init, copy to var, access undefined field
const Point = struct {
    x: u8,
    y: u8,
};

const c: Point = .{ .x = 5, .y = undefined };

fn copy_from_const(dst: *Point) void {
    dst.* = c;
}

pub fn main() u8 {
    var v: Point = undefined;
    copy_from_const(&v);
    return v.y;
}
