pub fn main() u8 {
    var x: ?u8 = undefined;
    x = 5; // store sets non_null
    return x.?; // OK: assigned non-null value
}
