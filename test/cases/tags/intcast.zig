// Test that @intCast doesn't crash the analyzer
pub fn main() u8 {
    const a: u32 = 300;
    const b: u8 = @intCast(a & 0xFF);
    return b;
}
