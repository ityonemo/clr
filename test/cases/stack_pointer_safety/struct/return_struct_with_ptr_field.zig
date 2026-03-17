// Tests returning a struct that contains a pointer field copied from an argument.
// This pattern appears in ArrayList.initCapacity where the Allocator's ptr field
// is copied into the returned struct.

const Container = struct {
    ptr: *u8,
    value: u32,
};

fn createContainer(ptr: *u8) Container {
    // ptr is an argument - copying it into a returned struct should NOT be
    // flagged as "stack pointer escape" - the pointer came from the caller.
    return Container{
        .ptr = ptr,
        .value = 42,
    };
}

pub fn main() u8 {
    var data: u8 = 100;
    const container = createContainer(&data);
    if (container.ptr.* != 100) return 1;
    if (container.value != 42) return 2;
    return 0;
}
