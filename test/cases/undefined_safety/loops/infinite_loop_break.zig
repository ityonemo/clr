// while(true) with conditional break - tests loops without cond_br at top
pub fn main() u8 {
    var x: u8 = undefined;
    var i: u8 = 0;
    while (true) {
        x = i;
        i += 1;
        if (i >= 10) break;
    }
    return x; // should be defined
}
