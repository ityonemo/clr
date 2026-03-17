// Tests that storing a struct into stack storage properly propagates pointer metadata.
// This is the pattern from ArrayList.initCapacity:
//   var self = Self.init(gpa);  // store returned struct into alloc'd storage
//   return self;
// Without proper struct-to-struct store handling, pointer fields in the returned
// struct would incorrectly retain stack metadata from alloc's setStackRecursive.

const Container = struct {
    ptr: *u8,
    value: u32,
};

fn createFromArg(ptr: *u8) Container {
    // The ptr argument comes from caller - returning it should NOT be stack escape
    var container: Container = undefined;
    container = Container{ .ptr = ptr, .value = 42 };
    return container;
}

pub fn main() u8 {
    var data: u8 = 100;
    const container = createFromArg(&data);
    if (container.ptr.* != 100) return 1;
    if (container.value != 42) return 2;
    return 0;
}
