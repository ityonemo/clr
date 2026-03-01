pub fn main() u8 {
    var callback: *const fn () u8 = undefined;
    _ = &callback;
    return callback(); // ERROR: calling undefined function pointer
}
