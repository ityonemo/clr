pub fn main() u8 {
    var x: ?u8 = null;
    _ = &x;
    if (x != null) {
        x = 5;
    }
    // x is ambiguous here: could be null (if branch not taken) or 5 (if taken)
    return x.?; // ERROR: unchecked unwrap of ambiguous optional
}
