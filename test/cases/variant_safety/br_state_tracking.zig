// Test br handler in variant_safety preserves state through labeled breaks
const Union = union(enum) { a: u8, b: u8 };

pub fn main() u8 {
    var u: Union = undefined;

    const val: u8 = blk: {
        u = .{ .a = 42 }; // set variant in block
        break :blk u.a;
    };

    return val;
}
