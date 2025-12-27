pub fn main() u8 {
    var x: ?u8 = 5;
    _ = &x;
    if (x == null) {
        return 0;
    }
    return x.?; // OK: in the else branch, x is non_null
}
