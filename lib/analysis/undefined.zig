pub const Meta = struct {
    file: ?[]const u8 = null,
    line: ?u32 = null,
    column: ?u32 = null,
    var_name: ?[]const u8 = null,
};

pub const Undefined = union {
    defined: void,
    undefined: Meta,
};