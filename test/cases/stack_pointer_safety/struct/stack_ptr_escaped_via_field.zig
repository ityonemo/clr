const Container = struct {
    ptr: *u8,
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
