const Container = union(enum) {
    ptr: *const u8,
    value: u8,
};

noinline fn escaped_param_ptr(param: u8) Container {
    return .{ .ptr = &param }; // Error: parameter pointer escapes in union
}

pub fn main() u8 {
    const container = escaped_param_ptr(20);
    return container.ptr.*;
}
