// Tests that returning a local by value after modifying it via pointer is not a false positive.
// Pattern from std.ArrayList.initCapacity:
//   var self = Self.init(gpa);
//   try self.ensureTotalCapacityPrecise(num);  // takes &self
//   return self;  // return by VALUE

const MyStruct = struct {
    value: u32,

    pub fn init() MyStruct {
        return .{ .value = 0 };
    }

    pub fn modify(self: *MyStruct) void {
        self.value = 42;
    }
};

pub fn initAndModify() MyStruct {
    var self = MyStruct.init();
    self.modify();  // Takes &self implicitly
    return self;    // Return by value - NOT a stack escape
}

pub fn main() u8 {
    const result = initAndModify();
    if (result.value != 42) return 1;
    return 0;
}
