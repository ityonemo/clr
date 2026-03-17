const Container = union(enum) { value: u32, other: u8 };

pub fn main() void {
    var c = Container{ .value = 42 };
    const parent: *Container = @fieldParentPtr("value", &c.value);
    _ = parent.value;
}
