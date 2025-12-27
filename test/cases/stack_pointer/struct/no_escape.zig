const Container = struct {
    ptr: *u8,
};

fn wrapInStruct(ptr: *u8) Container {
    ptr.* += 1;
    return .{ .ptr = ptr }; // OK: passed-in pointer, not local
}

pub fn main() u8 {
    var x: u8 = 10;
    const container = wrapInStruct(&x);
    return container.ptr.*;
}
