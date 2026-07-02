const Child = struct {
    sec: i64,
    nsec: u32,
    padding: u32,
};

const Parent = struct {
    first: Child,
    second: Child,
    third: Child,
};

pub fn main() void {
    const value = Parent{
        .first = .{ .sec = 1, .nsec = 2, .padding = 0 },
        .second = .{ .sec = 3, .nsec = 4, .padding = 0 },
        .third = .{ .sec = 5, .nsec = 6, .padding = 0 },
    };
    _ = value.second.nsec;
}
