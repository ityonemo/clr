pub const Meta = struct {
    file: ?[]const u8 = null,
    line: ?u32 = null,
    column: ?u32 = null,
    var_name: ?[]const u8 = null,
};

pub const Undefined = union(enum) {
    defined: void,
    undefined: Meta,

    pub fn reportUseBeforeAssign(self: Undefined, ctx: anytype) error{UseBeforeAssign} {
        _ = self;
        const func_name = ctx.stacktrace.items[ctx.stacktrace.items.len - 1];
        ctx.print("use of undefined value found in {s} ({s}:{d}:{d})\n", .{ func_name, ctx.file, ctx.line, ctx.column });
        return error.UseBeforeAssign;
    }
};