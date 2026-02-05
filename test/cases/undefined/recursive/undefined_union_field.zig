const Expr = union(enum) {
    number: u8,
    negate: *Expr,
};

pub fn main() u8 {
    var expr: Expr = undefined;
    expr = .{ .number = 42 };
    // This is fine - number is defined
    return expr.number;
}
