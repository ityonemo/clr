const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    var node: Node = undefined;
    node.next = null; // Only set next, not value
    return node.value; // ERROR: value is undefined
}
