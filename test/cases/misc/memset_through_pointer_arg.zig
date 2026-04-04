// Test case: memset through pointer argument should propagate defined state to caller
// This is a minimal reproduction of the HashMap false positive where initMetadatas
// memsets metadata through a pointer arg, but the caller still sees it as undefined.

pub fn main() u8 {
    var data: [4]u8 = undefined;

    // Call a function that memsets the array through a pointer
    initData(&data);

    // After initData returns, data should be defined
    // Accessing it should NOT trigger "use of undefined value"
    return data[0];
}

fn initData(ptr: *[4]u8) void {
    @memset(ptr, 0);
}
