// Tests passing undefined array element to function
fn use_value(val: u8) u8 {
    return val;
}

pub fn main() u8 {
    var arr: [3]u8 = undefined;
    _ = &arr;
    return use_value(arr[0]); // Error: passing undefined value
}
