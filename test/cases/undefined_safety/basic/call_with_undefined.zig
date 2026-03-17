fn add_one(x: u8) u8 {
    return x + 1;
}

pub fn main() u8 {
    var x: u8 = undefined;
    x = add_one(x);
    return x;
}
