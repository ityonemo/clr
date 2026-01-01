const Value = union(enum) {
    int: i32,
    float: f32,
};

pub fn main() u8 {
    var v: Value = .{ .int = 0 };
    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            v = .{ .float = 3.14 }; // changes to float
        },
        1 => {
            // doesn't change, still .int
        },
        else => {
            // doesn't change, still .int
        },
    }

    // Error: accessing .int but could be .float from case 0
    const i = v.int;
    _ = i;
    return 0;
}
