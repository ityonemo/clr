const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn setContainer(container: *Container) void {
    var x: u8 = 42;
    container.* = .{ .ptr = &x }; // Error: stack ptr escapes through out param
}

pub fn main() u8 {
    var container: Container = undefined;
    setContainer(&container);
    return container.ptr.*;
}
