const Container = union(enum) {
    ptr: *u8,
    value: u8,
};

fn wrapInUnion(ptr: *u8) Container {
    ptr.* += 1;
    return .{ .ptr = ptr }; // OK: passed-in pointer, not local
}

pub fn main() u8 {
    var x: u8 = 10;
    const container = wrapInUnion(&x);
    return container.ptr.*;
}
