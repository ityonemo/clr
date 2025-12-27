pub fn main() u8 {
    var x: ?u8 = 5;
    _ = &x;
    if (x != null) {
        return x.?; // OK: checked
    }
    return 0;
}
