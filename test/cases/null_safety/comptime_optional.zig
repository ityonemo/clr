pub fn main() u8 {
    const x: ?u8 = 5;
    return x.?; // OK: comptime known non-null
}
