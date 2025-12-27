pub fn main() u8 {
    var x: ?u8 = undefined;
    x = null; // store sets null
    return x.?; // ERROR: known null
}
