noinline fn escaped_param_ptr(param: u8) *const u8 {
    return &param;
}

pub fn main() u8 {
    const x = escaped_param_ptr(20);
    return x.*;
}
