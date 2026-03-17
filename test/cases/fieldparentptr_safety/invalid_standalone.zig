const Container = struct { value: u32 };

pub fn main() void {
    var standalone: u32 = 42;
    _ = @as(*Container, @fieldParentPtr("value", &standalone));
}
