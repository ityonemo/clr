const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn getContainer() Container {
    var x: u8 = 42;
    return .{ .ptr = &x }; // Error: stack ptr escapes in union
}

pub fn main() u8 {
    const container = getContainer();
    return container.ptr.*;
}
