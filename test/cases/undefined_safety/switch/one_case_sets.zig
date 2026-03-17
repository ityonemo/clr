pub fn main() u8 {
    var x: u8 = undefined;
    var selector: u8 = 0;
    _ = &selector;

    switch (selector) {
        0 => {
            x = 5;
        },
        1 => {
            // doesn't set x
        },
        else => {
            // doesn't set x
        },
    }
    // x may still be undefined if selector != 0
    return x;
}
