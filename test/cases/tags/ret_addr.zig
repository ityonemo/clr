// Test that @returnAddress() doesn't crash the analyzer
pub fn main() u8 {
    const addr = @returnAddress();
    _ = addr;
    return 0;
}
