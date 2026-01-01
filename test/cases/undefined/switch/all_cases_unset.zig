pub fn main() u8 {
    var x: u8 = 42;
    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            x = undefined;
        },
        1 => {
            x = undefined;
        },
        else => {
            x = undefined;
        },
    }
    // x is undefined in all cases
    return x;
}
