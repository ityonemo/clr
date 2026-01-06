// Tests array of pointers - setting one element defines all (uniform model)
pub fn main() u8 {
    var val: u8 = 42;
    var ptrs: [3]*u8 = undefined;
    ptrs[1] = &val;
    return ptrs[0].*; // OK: uniform model - setting any element defines all
}
