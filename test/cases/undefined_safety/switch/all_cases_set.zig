pub fn main() u8 {
    var x: u8 = undefined;
    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            x = 5;
        },
        1 => {
            x = 10;
        },
        else => {
            x = 15;
        },
    }
    // x is always set
    return x;
}
