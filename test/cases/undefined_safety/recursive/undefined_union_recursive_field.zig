const Expr = union(enum) {
    number: u8,
    negate: *Expr,
};

pub fn main() u8 {
    var inner: Expr = undefined;
    inner = .{ .number = undefined }; // Tag set, payload undefined
    const outer: Expr = .{ .negate = &inner };
    // Accessing undefined payload through recursive pointer
    return outer.negate.number;
}
