const ContainerA = struct { x: u32 };
const ContainerB = struct { x: u32 };

pub fn main() void {
    var a = ContainerA{ .x = 100 };
    _ = @as(*ContainerB, @fieldParentPtr("x", &a.x));
}
