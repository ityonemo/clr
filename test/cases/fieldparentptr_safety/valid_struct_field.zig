const Container = struct { value: u32 };

pub fn main() void {
    var c = Container{ .value = 42 };
    const parent: *Container = @fieldParentPtr("value", &c.value);
    _ = parent.value;
}
