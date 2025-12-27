pub fn main() u8 {
    var x: ?u8 = null;
    _ = &x;
    return x.?; // ERROR: unchecked unwrap
}
