const Container = struct {
    ptr: *u8,
};

fn getContainer() Container {
    var x: u8 = 42;
    return .{ .ptr = &x }; // Error: stack ptr escapes in struct
}

pub fn main() u8 {
    const container = getContainer();
    return container.ptr.*;
}
