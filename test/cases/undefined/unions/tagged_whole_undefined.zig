const Value = union(enum) {
    int: u8,
    float: f32,
};

pub fn main() u8 {
    var v: Value = undefined;
    _ = &v; // prevent "never mutated" error
    return v.int; // Error: whole union is undefined
}
