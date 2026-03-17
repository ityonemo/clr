// Tests array of pointers - dereferencing undefined pointer element
pub fn main() u8 {
    var ptrs: [3]*u8 = undefined;
    _ = &ptrs;
    return ptrs[0].*; // Error: pointer element is undefined
}
