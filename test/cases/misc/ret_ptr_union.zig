// Test ret_ptr/ret_load with union return
// Tagged unions are returned via ret_ptr/ret_load pattern:
//   %N = ret_ptr(*Container)     -- Get pointer to return storage
//   ... operations on %N ...     -- Write to return value
//   ret_load(%N)                 -- Return by loading from ret_ptr

const Container = union(enum) {
    value: u8,
    other: u16,
};

fn getValue() Container {
    return .{ .value = 42 };
}

pub fn main() u8 {
    const c = getValue();
    return c.value;
}
