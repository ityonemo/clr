pub fn returns_from_cases(selector: u8) u8 {
    var x: u8 = undefined;
    _ = &x;

    switch (selector) {
        0 => {
            return 42; // defined
        },
        1 => {
            return x; // undefined
        },
        else => {
            return 0; // defined
        },
    }
}

pub fn main() u8 {
    var selector: u8 = 1;
    _ = &selector;
    return returns_from_cases(selector);
}
