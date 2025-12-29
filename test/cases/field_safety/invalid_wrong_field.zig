const Container = struct { x: u32, y: u32 };

pub fn main() void {
    var c = Container{ .x = 1, .y = 2 };
    _ = @as(*Container, @fieldParentPtr("y", &c.x)); // pointer is from .x, claims .y
}
